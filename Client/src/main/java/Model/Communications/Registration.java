package Model.Communications;

//Classe responsável por gerar e enviar um pedido de registo

import com.google.gson.Gson;

import java.net.Socket;

public class Registration extends CommunicationType {

    private Gson gson;

    public Registration(){
        this.gson = new Gson();
    }

    public void sendRegistrationReq(String name, String pass, String dom, Socket socket){
        RegistrationReqObj tmp = new RegistrationReqObj(name, pass, dom);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }

    private static class RegistrationReqObj{

        private final String version = "1.0.0";
        private final String RequestType = "Registration";
        private final String Name;
        private final String Passoword;
        private final String Domicile;

        public RegistrationReqObj(String name, String pass, String dom){
            this.Name = name;
            this.Passoword = pass;
            this.Domicile = dom;
        }
    }
}
