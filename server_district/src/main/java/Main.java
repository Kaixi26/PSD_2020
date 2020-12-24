import Models.CommunicationProtocols.Responses.ContactWithInfected;
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
        /*if (args.length < 1) {
            System.out.println("Insufficient number of Arguments!");
            System.exit(1);
        }*/
        Gson gson = new Gson();
        Set<String> s = new HashSet<>();
        s.add("1");
        s.add("2");
        s.add("3");
        ContactWithInfected n = new ContactWithInfected(s);
        String json = gson.toJson(n);
        System.out.println(json);
        /*String user = "";
        ClientsLocationManager clientsLocationManager = new ClientsLocationManager();
        for(int i = 0; i<10;i++){
            user = String.valueOf(i);
            new Cenas( user, clientsLocationManager).start();
        }
        Thread.sleep(1000);
        System.out.println(clientsLocationManager.toString());*/
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
