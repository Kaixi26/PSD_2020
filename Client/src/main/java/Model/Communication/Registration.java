package Model.Communication;

//Classe responsável por gerar e enviar um pedido de registo

import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Properties;

public class Registration {

    private Gson gson;
    private String ip;
    private int port;

    public Registration(){
        this.gson = new Gson();
        this.loadConfigFile();
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

    public void sendRegistrationReq(String name, String pass, String dom){
        RegistrationReqObj tmp = new RegistrationReqObj(name,pass,dom);
        String json = gson.toJson(tmp);
        try{
            Socket socket = new Socket(ip, port);
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            out.println(json);
            out.close();
            socket.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    class RegistrationReqObj{

        private String version = "1.0.0";
        private String name;
        private String password;
        private String domicile;

        public RegistrationReqObj(String name, String pass, String dom){
            this.name = name;
            this.password = pass;
            this.domicile = dom;
        }
    }
}
