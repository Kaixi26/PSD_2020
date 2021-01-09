import Model.Communications.Communication;
import Model.Communications.DistrictServers.Subscriber;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZThread;

import java.util.Random;

import static java.lang.Thread.sleep;

public class teste {
    public static void main(String[] args) throws Exception{
        Communication com = new Communication();
        com.subConcentrationInc("Braga",1,2);
        while(true)
            System.out.println(com.receiveNotification());
    }
}
