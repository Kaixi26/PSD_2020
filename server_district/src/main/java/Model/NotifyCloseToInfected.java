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
public class NotifyCloseToInfected extends CommunicationType {
    
    private Gson gson;

    public NotifyCloseToInfected(){
        this.gson = new Gson();
    }
    
    
     public void sendNotifyCloseToInfected(String date,Socket socket){
        NotifyCloseToInfectedObj tmp = new NotifyCloseToInfectedObj(date);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }
    
    private static class NotifyCloseToInfectedObj{

        private final String version = "1.0.0";
        private final String RequestType = "CloseToInfected";
        private final String Date;

        public NotifyCloseToInfectedObj(String date){
            this.Date=date;  
        }
    }
}
