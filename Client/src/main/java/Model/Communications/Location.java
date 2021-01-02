package Model.Communications;

import com.google.gson.Gson;

import java.net.Socket;

public class Location extends CommunicationType{
    private Gson gson;

    public Location(){
        this.gson = new Gson();
    }

    public void sendNotifyLocationReq(int latitude, int longitude, Socket socket){
        NotifyLocationReqObj tmp = new NotifyLocationReqObj(latitude,longitude);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }

    public void sendProbeLocationReq(int latitude, int longitude, Socket socket){
        ProbeLocationReqObj tmp = new ProbeLocationReqObj(latitude,longitude);
        String json = gson.toJson(tmp);
        sendRequest(json,socket);
    }

    //Método que espera pela resposta do servidor front-end aṕos ser feito o pedido de NotifyLocation
    public boolean receiveNotifyLocationRes(Socket socket){
        String json = receiveResponse(socket);
        NotifyLocationResObj res = gson.fromJson(json, NotifyLocationResObj.class);
        return res.code.equals("200"); //TODO TEST + code
    }

    //Método que espera pela resposta do servidor front-end aṕos ser feito o pedido de NotifyLocation
    public int receiveProbeLocationRes(Socket socket){
        String json = receiveResponse(socket);
        ProbeLocationResObj res = gson.fromJson(json, ProbeLocationResObj.class);
        if(res.code.equals("200"))
            return res.clientsNumber;
        else
            return -1; //TODO TEST + code
    }

    //Objeto que representa um pedido de NotifyLocation
    private static class NotifyLocationReqObj {

        private final String version = "1.0.0";
        private final String RequestType = "NotifyLocation";
        private final int Latitude;
        private final int Longitude;

        public NotifyLocationReqObj(int latitude, int longitude){
            this.Latitude = latitude;
            this.Longitude = longitude;
        }
    }

    //Objeto que representa uma resposta de um pedido NotifyLocation
    private static class NotifyLocationResObj {

        private final String version = "1.0.0";
        private final String ResponseType = "NotifyLocation";
        private String code;

    }

    //Objeto que representa uma resposta a um pedido ProbeLocation
    private static class ProbeLocationResObj {

        public String version = "1.0.0";
        public String ResponseType = "ProbeLocation";
        public String code;
        public int clientsNumber;

    }

    //Objeto que representa um pedido ProbeLocation
    private static class ProbeLocationReqObj {

        private final String version = "1.0.0";
        private final String RequestType = "ProbeLocation";
        private final int Latitude;
        private final int Longitude;

        public ProbeLocationReqObj(int latitude, int longitude){
            this.Latitude = latitude;
            this.Longitude = longitude;
        }
    }
}
