/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Model;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;

/**
 *
 * @author jpedro
 */
public class CommunicationType {
        public CommunicationType(){
    }


    public void sendRequest(String json, Socket socket){
        try{
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            out.println(json);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
