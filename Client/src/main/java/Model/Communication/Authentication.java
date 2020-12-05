package Model.Communication;

//Classe responsável por gerar, enviar e receber um pedido de autenticação

import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Properties;

public class Authentication {
    private Gson gson;
    private String ip;
    private int port;

    public Authentication(){
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

    public void sendAuthenticationReq(String name, String pass){
        AuthenticationReqObj tmp = new AuthenticationReqObj(name,pass);
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

    class AuthenticationReqObj{

        private String version = "1.0.0";
        private String name;
        private String password;

        public AuthenticationReqObj(String name, String pass){
            this.name = name;
            this.password = pass;
        }
    }
}
