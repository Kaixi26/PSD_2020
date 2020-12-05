package Model.Communications;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.Properties;

public class Communication {
    private String ip;
    private int port;
    private Socket socket;

    public Communication(){
        try {
            loadConfigFile();
            this.socket = new Socket(ip,port);
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
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public void authenticate(String name,String pass){
        new Authentication().sendAuthenticationReq(name,pass,socket);
    }

    public void register(String name,String pass,String dom){
        new Registration().sendRegistrationReq(name,pass,dom,socket);
    }
}
