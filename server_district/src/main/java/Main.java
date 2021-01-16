import Auxiliar.DistrictServerConfigurations;
import Auxiliar.FrontendConnection;
import Business.MasterManager;
import Models.CommunicationProtocols.Requests.AnnounceDistrictServerRequest;
import Models.CommunicationProtocols.Responses.AnnounceDistrictServerResponse;
import Services.DirectoryPoster;
import Services.PublicNotificationsSender;
import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Properties;

public class Main {
    /**
     * @param districtName
     * @return DistrictServerCon
     * @throws IOException
     */
    public static DistrictServerConfigurations loadConfigurationsFile(String districtName) throws IOException {
        InputStream input = new FileInputStream("src/main/resources/config.properties");
        Properties prop = new Properties();
        prop.load(input);

        String frontend_ip = prop.getProperty("Server.frontend.ip");
        int frontend_port = Integer.parseInt(prop.getProperty("Server.frontend.port"));

        String directory_domainURL = prop.getProperty("Directory.domainURL");

        String public_notifications_ip = prop.getProperty("Public_notifications.ip");
        int public_notifications_port = Integer.parseInt(prop.getProperty("Public_notifications.port"));

        int district_dimension = Integer.parseInt(prop.getProperty("District.dimension." + districtName.toLowerCase().replace(" ","_")));
        String server_district_ip = prop.getProperty("Server.district.ip." + districtName.toLowerCase().replace(" ","_"));
        int server_district_port = Integer.parseInt(prop.getProperty("Server.district.port." + districtName.toLowerCase().replace(" ","_")));

        return new DistrictServerConfigurations(districtName, frontend_ip, frontend_port, directory_domainURL, public_notifications_ip, public_notifications_port, district_dimension, server_district_ip, server_district_port);
    }


    /**
     *
     * @param args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException, URISyntaxException {
        if(args.length < 1) {
            System.out.println("Insufficient Arguments");
            System.exit(1);
        }

        StringBuilder sb = new StringBuilder();
        for(int i = 0; i < args.length ; i++) {
            sb.append(args[i].trim());
            if(args.length-i != 1){
                sb.append(" ");
            }
        }
        String districtName = sb.toString();
        System.out.println("District Name: " + districtName);

        DistrictServerConfigurations configurations = loadConfigurationsFile(districtName);
        AnnounceDistrictServerRequest announceDistrictServerRequest = new AnnounceDistrictServerRequest(districtName, configurations.getDistrictServerIP(),
                                                                            configurations.getDistrictServerPort());

        Gson gson = new Gson();
        FrontendConnection frontendConnection = new FrontendConnection(configurations);
        frontendConnection.writeLine(gson.toJson(announceDistrictServerRequest));

        String responseLine;
        while ((responseLine = frontendConnection.readLine()) != null) {
            AnnounceDistrictServerResponse announceResponse = gson.fromJson(responseLine, AnnounceDistrictServerResponse.class);

            if (announceResponse.getStatusCode() >= 200 && announceResponse.getStatusCode() < 300) {
                System.out.println("Connection Accepted");
                break;
            } else {
                System.out.println("Connection Refused");
                System.exit(1);
            }
        }

        PublicNotificationsSender notificationsSender = new PublicNotificationsSender(configurations, gson);
        DirectoryPoster directoryPoster = new DirectoryPoster(configurations, gson);
        MasterManager manager = new MasterManager(configurations, notificationsSender, directoryPoster);
        new FrontendReader(frontendConnection, manager, gson).start();
    }
}
