package Model.Communications;

import java.io.*;
import java.net.Socket;
import java.util.Properties;

public abstract class CommunicationType {

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

    public String receiveResponse(Socket socket){
        try {
            BufferedReader in = new BufferedReader(
                    new InputStreamReader(socket.getInputStream()));
            String json = in.readLine();
            return json;
        } catch (IOException ex) {
            ex.printStackTrace();
            return "error";
        }
    }
}
