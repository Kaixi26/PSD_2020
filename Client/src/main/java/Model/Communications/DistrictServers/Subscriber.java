package Model.Communications.DistrictServers;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZThread;

public class Subscriber {

    private String ip;
    private String port;
    public ZMQ.Socket socket;

    public Subscriber(String ip, String port){
        this.ip = ip;
        this.port = port;
        ZContext context = new ZContext();
        this.socket = context.createSocket(SocketType.SUB);
        socket.connect("tcp://" + ip + ":" + port);
    }

    public boolean subscribe(String s){
        System.out.println("Subscribred to:" + s);
        return socket.subscribe(s.getBytes());
    }

    //Método que subscreve a notificação de aumento da concentração de pessoas numa localização de um certo distrito
    public boolean subConcentrationInc(String district, int lat, int lon){
        String str = district + "(" + lat + "," + lon + ")_ConcentrationIncreaseInLocation";
        return this.subscribe(str);
    }

    //Método que subscreve a notificação de diminuição da concentração de pessoas numa localização de um certo distrito
    public boolean subConcentrationDec(String district, int lat, int lon){
        String str = district + "(" + lat + "," + lon + ")_ConcentrationDecreaseInLocation";
        return this.subscribe(str);
    }

    //Método que subscreve a notificação de quando deixa de haver pessoas numa certa localização de um certo distrito
    public boolean subNobodyInLocation(String district, int lat, int lon) {
        String str = district + "(" + lat + "," + lon + ")_NobodyInLocation";
        return this.subscribe(str);
    }

    //Método que subscreve a notificação de occorrência de mais um infetado num dado distrito
    public boolean subInfectionsIncrease(String district){
        String str = district + "_InfectionsIncrease";
        return this.subscribe(str);
    }

    public boolean unsubConcentrationInc(String district, int lat, int lon){
        String str = district + "(" + lat + "," + lon + ")_ConcentrationIncreaseInLocation";
        return socket.unsubscribe(str);
    }

    public boolean unsubConcentrationDec(String district, int lat, int lon){
        String str = district + "(" + lat + "," + lon + ")_ConcentrationDecreaseInLocation";
        return socket.unsubscribe(str);
    }

    public boolean unsubNobodyInLocation(String district, int lat, int lon) {
        String str = district + "(" + lat + "," + lon + ")_NobodyInLocation";
        return socket.unsubscribe(str);
    }

    public boolean unsubInfectionsIncrease(String district){
        String str = district + "_InfectionsIncrease";
        return socket.unsubscribe(str);
    }


    //Método que recebe notificações vindas do broker, só retorna o json que vem nestas notificações
    public String receive(){
        return socket.recvStr().split(" ")[1];
    }



}
