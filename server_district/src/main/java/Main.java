import Models.CommunicationProtocols.Responses.ProbeLocationResponse;
import Models.Location;
import Business.ClientsLocationManager;
import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

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
        ProbeLocationResponse l =  new ProbeLocationResponse(200,12);
        System.out.println(gson.toJson(l));
        var l1 = gson.fromJson(gson.toJson(l), ProbeLocationResponse.class);
        System.out.println(l1.getStatusCode());
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
