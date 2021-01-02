import Auxiliar.DistrictServerConfigurations;
import Auxiliar.FrontendConnection;
import Business.MasterManager;
import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import Models.CommunicationProtocols.Requests.ProbeLocationRequest;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import org.zeromq.ZContext;

import java.io.IOException;

public class TaskManager extends Thread {
    private Gson gson;
    private FrontendConnection frontendConnection;
    private  DistrictServerConfigurations configurations;
    private MasterManager manager;

    public TaskManager(Gson gson, FrontendConnection frontendConnection, DistrictServerConfigurations configurations, ZContext context) {
        this.gson = gson;
        this.frontendConnection = frontendConnection;
        this.configurations = configurations;
        this.manager = new MasterManager(context, configurations.getDistrictName(), this.configurations.getDistrictDimension());
    }

    @Override
    public void run(){
        String jsonRequest, jsonResponse, requestType;
        Object requestModel, responseModel = null;
        JsonObject jObj;

        try {
            while ((jsonRequest = this.frontendConnection.readLine()) != null) {

                jObj = this.gson.fromJson(jsonRequest, JsonObject.class);
                requestType = jObj.get("RequestType").getAsString();

                switch (requestType){
                    case "NotifyInfection":
                        requestModel = this.gson.fromJson(jsonRequest, NotifyInfectionRequest.class);
                        responseModel = this.manager.clientInfected((NotifyInfectionRequest) requestModel);
                        break;

                    case "NotifyLocation":
                        requestModel = this.gson.fromJson(jsonRequest, NotifyLocationRequest.class);
                        responseModel = this.manager.moveClientToLocation((NotifyLocationRequest) requestModel);
                        break;

                    case "ProbeLocation":
                        requestModel = this.gson.fromJson(jsonRequest, ProbeLocationRequest.class);
                        responseModel = this.manager.getNumberOfClientsInLocation((ProbeLocationRequest) requestModel);
                        break;
                    default:
                        System.out.println("Internal Server Error: The request type is invalid");
                        break;
                }
                jsonResponse = this.gson.toJson(responseModel);
                this.frontendConnection.writeLine(jsonResponse);
            }
        } catch (IOException e) {
            System.out.println("Internal Server Error : Something Went Wrong");
            e.printStackTrace();
        }
    }
}
