import Auxiliar.DistrictServerConfigurations;
import Auxiliar.FrontendConnection;
import Business.MasterManager;
import Models.CommunicationProtocols.Requests.AnnounceDistrictServerRequest;
import Models.CommunicationProtocols.Responses.AnnounceDistrictServerResponse;
import Services.PublicNotificationsSender;
import com.google.gson.Gson;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
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

        String directory_ip = prop.getProperty("Directory.ip");
        int directory_port = Integer.parseInt(prop.getProperty("Directory.port"));

        String public_notifications_ip = prop.getProperty("Public_notifications.ip");
        int public_notifications_port = Integer.parseInt(prop.getProperty("Public_notifications.port"));

        int district_dimension = Integer.parseInt(prop.getProperty("District.dimension." + districtName.toLowerCase()));
        String server_district_ip = prop.getProperty("Server.district.ip." + districtName.toLowerCase());
        int server_district_port = Integer.parseInt(prop.getProperty("Server.district.port." + districtName.toLowerCase()));

        return new DistrictServerConfigurations(districtName, frontend_ip, frontend_port, directory_ip, directory_port, public_notifications_ip, public_notifications_port, district_dimension, server_district_ip, server_district_port);
    }


    /**
     *
     * @param args
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        if(args.length < 1) {
            System.out.println("Insufficient Arguments");
            System.exit(1);
        }

        String districtName = args[0];
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
        MasterManager manager = new MasterManager(configurations, notificationsSender);
        new FrontendReader(frontendConnection, manager, gson).start();
    }
}
