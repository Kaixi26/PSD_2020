package Aux;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class ZeroMQ {
    private final int porta_send;
    private final int porta_pull;

    public ZeroMQ(int porta_send, int porta_pull) {
        this.porta_send = porta_send;
        this.porta_pull = porta_pull;
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB);
             ZMQ.Socket sink = context.createSocket(SocketType.PULL))
        {
            socket.bind("tcp://*:" +porta_send);
            sink.bind("tcp://*:" +porta_pull);

            byte [] msg;
            while (true) {
                msg = sink.recv();
                socket.send(msg);
            }

        }
    }
}