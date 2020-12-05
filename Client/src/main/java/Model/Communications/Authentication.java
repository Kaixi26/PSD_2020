package Model.Communications;

//Classe responsável por gerar, enviar e receber um pedido de autenticação

import com.google.gson.Gson;

import java.net.Socket;

public class Authentication extends CommunicationType {
    private Gson gson;

    public Authentication(){
        this.gson = new Gson();
    }


    public void sendAuthenticationReq(String name, String pass,Socket socket){
        AuthenticationReqObj tmp = new AuthenticationReqObj(name, pass);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }

    private static class AuthenticationReqObj{

        private final String version = "1.0.0";
        private final String RequestType = "Authentication";
        private final String Name;
        private final String Password;

        public AuthenticationReqObj(String name, String pass){
            this.Name = name;
            this.Password = pass;
        }
    }
}
