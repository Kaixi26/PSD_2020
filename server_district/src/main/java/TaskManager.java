import Auxiliar.FrontendConnection;
import Business.MasterManager;
import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import Models.CommunicationProtocols.Requests.ProbeLocationRequest;
import com.google.gson.Gson;
import com.google.gson.JsonObject;

public class TaskManager extends Thread {
    private FrontendConnection frontendConnection;
    private MasterManager manager;
    private Gson gson;
    private String jsonRequest;

    public TaskManager(FrontendConnection frontendConnection, MasterManager manager, Gson gson, String jsonRequest) {
        this.frontendConnection = frontendConnection;
        this.manager = manager;
        this.gson = gson;
        this.jsonRequest = jsonRequest;
    }

    @Override
    public void run() {
        Object requestModel, responseModel = null;
        JsonObject jObj = this.gson.fromJson(jsonRequest, JsonObject.class);
        String requestType = jObj.get("RequestType").getAsString();

        switch (requestType){
            case "NotifyLocation":
                System.out.println("Task Manager: NotifyLocation");
                requestModel = this.gson.fromJson(jsonRequest, NotifyLocationRequest.class);
                responseModel = this.manager.moveClientToLocation((NotifyLocationRequest) requestModel);
                break;

            case "ProbeLocation":
                System.out.println("Task Manager: ProbeLocation");
                requestModel = this.gson.fromJson(jsonRequest, ProbeLocationRequest.class);
                responseModel = this.manager.getNumberOfClientsInLocation((ProbeLocationRequest) requestModel);
                break;

            case "NotifyInfection":
                System.out.println("Task Manager: NotifyLocation");
                requestModel = this.gson.fromJson(jsonRequest, NotifyInfectionRequest.class);
                responseModel = this.manager.clientInfected((NotifyInfectionRequest) requestModel);
                break;

            default:
                System.out.println("Internal Server Error: The request type is invalid");
                break;
        }

        String jsonResponse = this.gson.toJson(responseModel);
        this.frontendConnection.writeLine(jsonResponse);

        try {
            Thread.currentThread().join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
