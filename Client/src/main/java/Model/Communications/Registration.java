package Model.Communications;

//Classe responsável por gerar e enviar um pedido de registo

import com.google.gson.Gson;

import java.net.Socket;

public class Registration {

    private final Gson gson;
    private final CommunicationHandler com;

    public Registration(CommunicationHandler com){
        this.gson = new Gson();
        this.com = com;
    }

    //Método que cria e envia um pedido em json ao servidor front-end no socket fornecido
    public void sendRegistrationReq(String name, String pass, String dom){
        RegistrationReqObj tmp = new RegistrationReqObj(name, pass, dom);
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    //Método que espera pela resposta do servidor front-end após o pedido de registo
    public boolean receiveRegistrationRes(){
        String json = com.receiveResponse();
        RegistrationResObj res = gson.fromJson(json,RegistrationResObj.class);
        return res.code.equals("201");
    }

    //Objeto que representa o json enviado ao servidor front-end quando é feito um registo
    private static class RegistrationReqObj{

        private final String version = "1.0.0";
        private final String RequestType = "Registration";
        private final String Name;
        private final String Password;
        private final String Domicile;

        public RegistrationReqObj(String name, String pass, String dom){
            this.Name = name;
            this.Password = pass;
            this.Domicile = dom;
        }
    }
    //Objeto que corresponde ao json recibido como resposta do servidor front-end após um pedido de registo
    private static class RegistrationResObj{ //{"version":"1.0.0","ReplyType":"Registration","code": 201}
        private final String version = "1.0.0";
        private final String ReplyType = "Registration";
        private String code;
    }
}
