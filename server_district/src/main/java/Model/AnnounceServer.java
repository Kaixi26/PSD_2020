/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Model;
import com.google.gson.Gson;
import java.net.Socket;

/**
 *
 * @author jpedro
 */
public class AnnounceServer extends CommunicationType {
    
    private Gson gson;

    public AnnounceServer(){
        this.gson = new Gson();
    }
    
    public void sendAnnounceServer(String district,Socket socket){
        AnnounceServerObj tmp = new AnnounceServerObj(district);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }
    
    
    private static class AnnounceServerObj{

        private final String version = "1.0.0";
        private final String RequestType = "AnnounceServer";
        private final String District;

        public AnnounceServerObj(String district){
            this.District=district;  
        }
    }
}
