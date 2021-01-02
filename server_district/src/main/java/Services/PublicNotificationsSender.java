package Services;

import Auxiliar.DistrictServerConfigurations;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class PublicNotificationsSender extends Thread {
    private ZContext context;
    private final String PUB_IP;
    private final int PUB_Port;

    public PublicNotificationsSender(ZContext context, DistrictServerConfigurations configurations) {
        this.context = context;
        this.PUB_IP = configurations.getPublicNotificationsIP();
        this.PUB_Port = configurations.getPublicNotificationsPort();
    }

    @Override
    public void run() {
        try (ZMQ.Socket socketPULL = context.createSocket(SocketType.PULL);
             ZMQ.Socket socketPUB = context.createSocket(SocketType.PUB)) {

            socketPULL.bind("inproc://notifications_workers");
            socketPUB.bind("tcp://"+ this.PUB_IP + ":" + this.PUB_Port);

            while (true) {
                byte[] notification = socketPULL.recv();
                System.out.println("Public Notification: " + new String(notification));
                socketPUB.send(notification);
            }
        }
    }
}
