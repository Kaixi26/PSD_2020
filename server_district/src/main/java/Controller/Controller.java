/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Controller;




import Model.Communication;
import View.View;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Controller {
    
    private Communication model;
    private View view;

    public Controller(){
        this.model = new Communication();
        this.view = new View();
    }

    public void init(){                       
        view.init(model.getDistrict());
        try {
        
        boolean alive = true;
        
       
        BufferedReader in =new BufferedReader(new InputStreamReader(model.getSocket().getInputStream()));
        String json;
        
        while(alive){

           json=in.readLine();
           JsonObject jobj = new Gson().fromJson(json, JsonObject.class);
           String RequestType= jobj.get("RequestType").getAsString();
            
           
           switch(RequestType){
               
               case "Infected":
                    model.NotifyCloseToInfected(json);
                    break;
           }
        
        }
    
         } catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
    }
    
    
    
    
    
     private static class ReceiveInfectedObj{

        private  String version;
        private  String RequestType;
        private  String User;
     }

}
