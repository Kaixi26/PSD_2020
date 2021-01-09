import Auxiliar.PublicNotificationsConfigurations;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Broker {
    public static PublicNotificationsConfigurations loadConfigurationsFile() throws IOException {
        InputStream input = new FileInputStream("src/main/resources/config.properties");
        Properties prop = new Properties();
        prop.load(input);

        String servers_district_public_notifications_ip = prop.getProperty("Public_notifications.servers_district.ip");
        int servers_district_public_notifications_port = Integer.parseInt(prop.getProperty("Public_notifications.servers_district.port"));

        String clients_public_notifications_ip = prop.getProperty("Public_notifications.clients.ip");
        int clients_public_notifications_port = Integer.parseInt(prop.getProperty("Public_notifications.clients.port"));

        return new PublicNotificationsConfigurations(servers_district_public_notifications_ip, servers_district_public_notifications_port, clients_public_notifications_ip, clients_public_notifications_port);
    }

    public static void main(String[] args) throws IOException {
        PublicNotificationsConfigurations configurations = loadConfigurationsFile();

        try(ZContext context = new ZContext();
            ZMQ.Socket pubs = context.createSocket(SocketType.XSUB);
            ZMQ.Socket subs = context.createSocket(SocketType.XPUB))
        {
            pubs.bind("tcp://" + configurations.getPublicNotificationsIP_ServersDistrict() + ":" + configurations.getPublicNotificationsPort_ServersDistrict());
            subs.bind("tcp://" + configurations.getPublicNotificationsIP_Clients() + ":" + configurations.getPublicNotificationsPort_Clients());
            new Proxy(context, pubs, subs).poll();
        }
    }
}
