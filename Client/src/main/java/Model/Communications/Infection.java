package Model.Communications;

import com.google.gson.Gson;

import java.net.Socket;
import java.util.List;

public class Infection extends CommunicationType {

    private final Gson gson;

    public Infection(){
        this.gson = new Gson();
    }

    //Método que envia ao servidor frontend a informação que o utilizador está infetado
    public void sendInfectedReq(Socket socket){
        InfectedReqObj tmp = new InfectedReqObj();
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }

    public boolean receiveInfectedRes(Socket socket){
        String json = receiveResponse(socket);
        InfectedResObj res = gson.fromJson(json, InfectedResObj.class);
        System.out.println(res.code);
        return res.code.equals("200");
    }

    //Objecto que representa o json enviado ao servidor front-end para notificar que o utilizador está doente
    public class InfectedReqObj {

        private final String version = "1.0.0";
        private final String RequestType = "NotifyInfection";

    }

    //Objecto que representa o json recebido como confirmação que marcou o user como doente
    public class InfectedResObj {
        private List contacts;
        private String version;
        public String ResponseType;
        public String code;
    }
}
