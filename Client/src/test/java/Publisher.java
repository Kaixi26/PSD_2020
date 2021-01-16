import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZThread;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Random;

public class Publisher {
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.connect("tcp://localhost:" + "12348");
            while (true) {
                String str = br.readLine();
                if (str == null) break;
                socket.send(str);
            }
        }
    }
}
