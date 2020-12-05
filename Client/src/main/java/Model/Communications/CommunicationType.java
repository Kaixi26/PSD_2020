package Model.Communications;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
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
}
