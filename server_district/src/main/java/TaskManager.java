import Business.MasterManager;
import com.google.gson.Gson;
import com.google.gson.JsonObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;

public class TaskManager extends Thread {
    private Gson gson;
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    private MasterManager manager;

    public TaskManager() throws IOException {
        this.gson = new Gson();
    }

    @Override
    public void run(){
        String jsonRequest, requestType;
        JsonObject jobj;



        try {
            while ((jsonRequest = in.readLine()) != null) {

                jobj = new Gson().fromJson(jsonRequest, JsonObject.class);
                requestType = jobj.get("RequestType").getAsString();

                switch (requestType){
                    case "NotifyLocation":

                        break;
                    case "ProbeLocation":

                        break;
                    default:
                        break;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
