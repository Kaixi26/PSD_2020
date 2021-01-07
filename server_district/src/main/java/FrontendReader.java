import Auxiliar.FrontendConnection;
import Business.MasterManager;
import com.google.gson.Gson;

import java.io.IOException;

public class FrontendReader extends Thread {
    private FrontendConnection frontendConnection;
    private MasterManager manager;
    private Gson gson;

    public FrontendReader(FrontendConnection frontendConnection, MasterManager manager, Gson gson) {
        this.frontendConnection = frontendConnection;
        this.manager = manager;
        this.gson = gson;
    }

    @Override
    public void run() {
        String jsonRequest;
        try {
            while ((jsonRequest = this.frontendConnection.readLine()) != null) {
                new TaskManager(this.frontendConnection, this.manager, this.gson, jsonRequest).start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
