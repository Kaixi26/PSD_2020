import Auxiliar.DistrictServerConfigurations;
import Auxiliar.FrontendConnection;
import Models.CommunicationProtocols.Requests.AnnounceDistrictServerRequest;
import Models.CommunicationProtocols.Responses.AnnounceDistrictServerResponse;
import Services.PublicNotificationsSender;
import com.google.gson.Gson;
import org.springframework.core.io.FileSystemResource;
import org.zeromq.ZContext;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.Socket;
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
        int district_dimension = Integer.parseInt(prop.getProperty("District.dimension." + districtName));
        String server_district_ip = prop.getProperty("District.server_ip." + districtName);
        int server_district_port = Integer.parseInt(prop.getProperty("District.server_port." + districtName));
        String public_notifications_ip = prop.getProperty("District.public_notifications_ip." + districtName);
        int public_notifications_port = Integer.parseInt(prop.getProperty("District.public_notifications_port." + districtName));
        return new DistrictServerConfigurations(districtName, frontend_ip, frontend_port, directory_ip, directory_port, district_dimension, server_district_ip, server_district_port, public_notifications_ip, public_notifications_port);
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

        DistrictServerConfigurations configurations = loadConfigurationsFile(districtName.toLowerCase());
        AnnounceDistrictServerRequest announceDistrictServerRequest = new AnnounceDistrictServerRequest(districtName, configurations.getDistrictServerIP(), configurations.getDistrictServerPort(), configurations.getPublicNotificationsIP(), configurations.getPublicNotificationsPort());

        Gson gson = new Gson();
        FrontendConnection frontendConnection = new FrontendConnection(configurations.getFrontendIP(), configurations.getFrontendPort());
        //Announcement
        frontendConnection.writeLine(gson.toJson(announceDistrictServerRequest));

        String responseLine;
        while ((responseLine = frontendConnection.readLine()) != null) {
            AnnounceDistrictServerResponse announceResponse = gson.fromJson(responseLine, AnnounceDistrictServerResponse.class);

            if (announceResponse.getStatusCode() >= 200 && announceResponse.getStatusCode() < 300) {
                System.out.println("Connection Accepted");
                final ZContext context = new ZContext();
                new PublicNotificationsSender(context, configurations.getPublicNotificationsIP(), configurations.getPublicNotificationsPort());
                new TaskManager(gson, frontendConnection, configurations, context).start();
            } else {
                System.out.println("Connection Refused");
                System.exit(1);
            }
        }
    }
}
