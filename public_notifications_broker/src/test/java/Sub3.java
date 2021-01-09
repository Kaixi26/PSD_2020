import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Sub3 {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.SUB))
        {
            socket.connect("tcp://127.0.0.1:12349");

            socket.subscribe("Braga(1,1)_ConcentrationDecreaseInLocation");

            while (true) {
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}
