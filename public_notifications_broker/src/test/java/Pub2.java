import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;

public class Pub2 {
    public static void main(String[] args) throws IOException {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.connect("tcp://127.0.0.1:12348");
            byte[] x = new byte[1024];
            while (true) {
                System.in.read(x);
                String str = new String(x);
                if (str == null) break;
                socket.send(str);
            }
        }
    }
}
