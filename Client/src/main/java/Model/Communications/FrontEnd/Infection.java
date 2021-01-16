package Model.Communications.FrontEnd;

import com.google.gson.Gson;

import java.util.List;

public class Infection {

    private final Gson gson;
    private CommunicationHandler com;

    public Infection(CommunicationHandler com){
        this.gson = new Gson();
        this.com = com;
    }

    //Método que envia ao servidor frontend a informação que o utilizador está infetado
    public void sendInfectedReq(){
        InfectedReqObj tmp = new InfectedReqObj();
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    public boolean receiveInfectedRes(){
        String json = com.receiveResponse();
        InfectedResObj res = gson.fromJson(json, InfectedResObj.class);
        return res.code.equals("201");
    }

    //Objecto que representa o json enviado ao servidor front-end para notificar que o utilizador está doente
    public class InfectedReqObj {

        private final String version = "1.0.0";
        private final String RequestType = "NotifyInfection";

    }

    //Objecto que representa o json recebido como confirmação que marcou o user como doente
    public class InfectedResObj { //{"contacts":["antonio","rui"],"ResponseType":"NotifyInfection","code":200,"version":"1.0.0"}
        private List contacts;
        private String version;
        public String ResponseType;
        public String code;
    }
}
