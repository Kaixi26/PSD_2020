import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class PublicNotificationsTest {
    public static void main(String[] args) {
        final ZContext context = new ZContext();
        ZMQ.Socket socketSUB = context.createSocket(SocketType.SUB);
        socketSUB.connect("tcp://127.0.0.1:12348");

        socketSUB.subscribe("Braga(1,1)_ConcentrationIncreaseInLocation");

        while (true){
            System.out.println(new String(socketSUB.recv()));
        }
    }
}
