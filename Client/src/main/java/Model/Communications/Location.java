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
