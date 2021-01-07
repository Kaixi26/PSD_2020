package Model.Communications;

//Classe responsável por gerar, enviar e receber um pedido de autenticação

import com.google.gson.Gson;


public class Authentication {
    private final Gson gson;
    private final CommunicationHandler com;

    public Authentication(CommunicationHandler com){
        this.gson = new Gson();
        this.com = com;
    }


    //Método que envia o pedido de autenticação ao servidor front-end
    public void sendAuthenticationReq(String name, String pass){
        AuthenticationReqObj tmp = new AuthenticationReqObj(name, pass);
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    //Método que espera pela resposta do servidor front-end aṕos ser feito o pedido de autenticação
    public boolean receiveAuthenticationRes(){
        String json = com.receiveResponse();
        AuthenticationResObj res = gson.fromJson(json,AuthenticationResObj.class);
        return res.code.equals("200");
    }

    //Objeto que corresponde ao json enviado ao servidor front-end em um pedido de autenticação
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

    //Objeto que corresponde ao json recebido após ter sido enviado um pedido de autenticação ao servidor front-end
    private static class AuthenticationResObj{ //{"version":"1.0.0","ReplyType":"Authentication","code": 200}

        public final String version = "1.0.0";
        public final String ReplyType = "Authentication";
        public String code;

    }
}
