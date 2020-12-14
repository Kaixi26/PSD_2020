/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Model;

import java.net.Socket;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import com.google.gson.Gson;
/**
 *
 * @author jpedro
 */
public class Communication {

    private String ip;
    private int port;
    private Socket socket;
    
    private String district;
    

    public Communication(){
        try {
            loadConfigFile();
            this.socket = new Socket(ip,port);
            new AnnounceServer().sendAnnounceServer(district,socket);
        }
        catch (Exception ex){
            ex.printStackTrace();
        }
    }

    private void loadConfigFile(){
        try (InputStream input = new FileInputStream("src/main/resources/config.properties")) {
            Properties prop = new Properties();
            prop.load(input);
            this.ip = prop.getProperty("Server.frontend.ip");
            this.port = Integer.parseInt(prop.getProperty("Server.frontend.port"));
            this.district=prop.getProperty("District.name");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
   
    public void NotifyCloseToInfected(String json){

        ReceiveInfectedObj tmp= new Gson().fromJson(json, ReceiveInfectedObj.class);
                
         //logica de ir ver os users que tiveram perto       
                
        String date="2017-24-28T04:24:27";
        new NotifyCloseToInfected().sendNotifyCloseToInfected(date,socket);
    }
    
    public String getDistrict(){
        return this.district;
    }
    public Socket getSocket(){
        return this.socket;
    }

    
    
    
    
    
     
     private static class ReceiveInfectedObj{

        private  String version;
        private  String RequestType;
        private  String User;
     }
}
