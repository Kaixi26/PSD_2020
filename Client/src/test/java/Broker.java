import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Broker {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket pubs = context.createSocket(SocketType.XSUB);
             ZMQ.Socket subs = context.createSocket(SocketType.XPUB))
        {
            pubs.bind("tcp://localhost:"+"12348");
            subs.bind("tcp://localhost:"+"1234");
            new Proxy(context, pubs, subs).poll();
        }
    }
}