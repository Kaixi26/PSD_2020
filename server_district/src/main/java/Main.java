import Models.CommunicationProtocols.Requests.AnnounceDistrictServerRequest;
import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import Models.CommunicationProtocols.Requests.ProbeLocationRequest;
import Models.CommunicationProtocols.Responses.AnnounceDistrictServerResponse;
import Models.CommunicationProtocols.Responses.NotifyInfectionResponse;
import Models.CommunicationProtocols.Responses.NotifyLocationResponse;
import Models.CommunicationProtocols.Responses.ProbeLocationResponse;
import Models.Location;
import Business.ClientsLocationManager;
import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

public class Main {
    private void loadConfigFile() {
        try (InputStream input = new FileInputStream("src/main/resources/config.properties")) {
            Properties prop = new Properties();
            prop.load(input);
            String frontend_ip = prop.getProperty("Server.frontend.ip");
            int frontend_port = Integer.parseInt(prop.getProperty("Server.frontend.port"));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        Gson gson = new Gson();
        //var request = gson.fromJson("{\"location\":{\"latitude\":12,\"longitude\":11},\"request_type\":\"ProbeLocation\",\"version\":\"1.0.0\"}", ProbeLocationRequest.class);
        var request = new AnnounceDistrictServerRequest("braga","127.0.9.2",5000,"180.12.43.2",80);
        System.out.println(gson.toJson(request));

        Set<String> s = new HashSet<>();
        s.add("antonio");
        s.add("rui");
        var response = new AnnounceDistrictServerResponse(200);
        System.out.println(gson.toJson(response));
    }


    public static class Cenas extends Thread{
        public ClientsLocationManager clientsLocationManager;
        public String user;

        public Cenas(String user,ClientsLocationManager clientsLocationManager){
            this.clientsLocationManager = clientsLocationManager;
            this.user = user;
        }

        @Override
        public void run() {
            for(int i = 0; i<10000 ; i++){
                clientsLocationManager.putClientLocation(user, new Location(i,i*2+3));
                if(i % 2 == 0){
                    Location l = clientsLocationManager.getClientLocation(user);
                    //System.out.println(user + "          "+ l.getLatitude() + "    " + l.getLongitude() +"\n");
                }
            }
        }
    }
}

        /*
        String json = "{\"latitude\":1,\"longitude\":4}";
        System.out.println(json);
        Location l = gson.fromJson(json, Location.class);
        System.out.println(l.getLatitude() + " " + l.getLongitude());*/


        //Location location = gson.fromJson(json, Location.class);
        //System.out.println(location.getLatitude() + " " + location.getLongitude());
        //NotifyLocation n = new NotifyLocation("eu",1,2);

        /*List<String> s = new ArrayList<>();
        s.add("1");
        s.add("2");
        s.add("3");
        Infecteds n = new Infecteds(s);
        json = gson.toJson(n);
        System.out.println(json);*/
