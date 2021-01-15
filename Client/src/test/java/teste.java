import Model.Communications.Communication;
import Model.Communications.DistrictServers.Subscriber;
import local.Subscriptions;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import static java.lang.Thread.sleep;

public class teste {
    public static void main(String[] args) throws Exception{
        Subscriptions subs = new Subscriptions();
        subs.add("Braga(1,2)_ConcentrationDecreaseInLocation");
        subs.add("Braga(1,2)_NobodyInLocation");
        subs.add("Braga(4,5)_NobodyInLocation");
        subs.add("Braga_InfectionsIncrease");
        subs.add("Porto(1,2)_ConcentrationDecreaseInLocation");
        subs.add("Porto(1,2)_NobodyInLocation");
        subs.add("Braga_InfectionsIncrease");
        subs.add("Beja(1,2)_NobodyInLocation");
        subs.remove("Braga(0,0)_ConcentrationDecreaseInLocation");
        subs.remove("Braga(0,0)_NobodyInLocation");
        subs.remove("Braga(4,5)_NobodyInLocation");
        subs.remove("Braga_InfectionsIncrease");
        subs.remove("Porto(1,2)_ConcentrationDecreaseInLocation");
        subs.remove("Porto(1,2)_NobodyInLocation");
        subs.remove("Beja(1,2)_NobodyInLocation");
        subs.remove("Braga_InfectionsIncrease");


        ZContext context = new ZContext();
        ZMQ.Socket socket = context.createSocket(SocketType.SUB);
        socket.connect("tcp://" + "localhost" + ":" + "1234");

        socket.subscribe("Braga(0,0)_ConcentrationIncreaseInLocation".getBytes());
        System.out.println(socket.recvStr());

        //System.out.println(subs.subs.toString());

    }
}
