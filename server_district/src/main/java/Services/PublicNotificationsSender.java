package Services;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class PublicNotificationsSender extends Thread {
    private ZContext context;
    private final String PUB_IP;
    private final int PUB_Port;

    public PublicNotificationsSender(ZContext context, String pubNotificationsIP, int pubNotificationsPort) {
        this.context = context;
        this.PUB_IP = pubNotificationsIP;
        this.PUB_Port = pubNotificationsPort;
    }

    @Override
    public void run() {
        try (ZMQ.Socket socketPULL = context.createSocket(SocketType.PULL);
             ZMQ.Socket socketPUB = context.createSocket(SocketType.PUB)) {

            socketPULL.bind("inproc://notifications_workers");
            socketPUB.bind("tcp://"+this.PUB_IP+":"+this.PUB_Port);

            while (true) {
                byte[] notification = socketPULL.recv();
                socketPUB.send(notification);
            }
        }
    }
}
