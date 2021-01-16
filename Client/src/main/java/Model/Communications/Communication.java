package Model.Communications;

import Model.Communications.DistrictServers.Subscriber;
import Model.Communications.FrontEnd.*;
import Model.Communications.local.DistrictLimitException;
import Model.Communications.local.SubscriptionObj;
import Model.Communications.local.Subscriptions;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class Communication {
    private String ip_front_end;
    private String port_front_end;
    private String ip_zeromq;
    private String port_zeromq;
    private CommunicationHandler com;
    private Subscriber sub;
    private Subscriptions subs;

    public Communication() throws IOException {
        loadConfigFile();
        Socket socket = new Socket(ip_front_end, Integer.parseInt(port_front_end));
        this.com = new CommunicationHandler(socket);
        this.sub = new Subscriber(ip_zeromq, port_zeromq);
        this.subs = new Subscriptions();
    }

    private void loadConfigFile() {
        try (InputStream input = new FileInputStream("src/main/resources/config.properties")) {
            Properties prop = new Properties();
            prop.load(input);
            this.ip_front_end = prop.getProperty("Server.frontend.ip");
            this.port_front_end = prop.getProperty("Server.frontend.port");
            this.ip_zeromq = prop.getProperty("Server.zeromq.ip");
            this.port_zeromq = prop.getProperty("Server.zeromq.port");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    //Método que faz uso do Garbage Collector para eliminar as subscrições feitas pelo utilizador anterior, ao fazer um novo "Subscriber" não existem subscrições e o
    // "Subscriber" da sessão anterior vai ser eliminado pelo garbage collector
    public void restartSubscriber() {
        this.sub = new Subscriber(ip_zeromq, port_zeromq);
    }

    //Método que autentica um utilizador, retorna true/false para sucesso/insucesso
    public boolean authenticate(String name, String pass) {
        Authentication a = new Authentication(com);
        a.sendAuthenticationReq(name, pass);
        return a.receiveAuthenticationRes();
    }

    //Método que faz logout ao utilizador, retorna true/false para sucesso/insucesso
    public boolean deAuthenticate() {
        Authentication a = new Authentication(com);
        a.sendDeAuthenticationReq();
        return a.receiveDeAuthenticationRes();
    }

    //Método que regista um utilizador, retorna true/false para sucesso/insucesso
    public boolean register(String name, String pass, String dom) {
        Registration r = new Registration(com);
        r.sendRegistrationReq(name, pass, dom);
        return r.receiveRegistrationRes();
    }

    //Método que atualiza a localização de um utilizador, retorna true/false para sucesso/insucesso
    public boolean position(int lat, int lon) {
        Location l = new Location(com);
        l.sendNotifyLocationReq(lat, lon);
        return l.receiveNotifyLocationRes();
    }

    //Método que obtém o número de pessoas numa dada localização do seu distrito
    public int probe(int lat, int lon) {
        Location l = new Location(com);
        l.sendProbeLocationReq(lat, lon);
        return l.receiveProbeLocationRes();
    }

    //Método que comunica ao servidor que o utilizador está infetado
    public boolean sick() {
        Infection i = new Infection(com);
        i.sendInfectedReq();
        return i.receiveInfectedRes();
    }

    public boolean contact() {
        return !com.receiveNotification().equals("error");
    }

    public void clearQueue() {
        com.clearNotificationQueue();
    }

    //Método que subscreve a notificação de aumento da concentração de pessoas numa localização de um certo distrito
    public boolean subConcentrationInc(String district, int lat, int lon) throws DistrictLimitException {
        this.addSubscription(new SubscriptionObj(district, "ConcentrationIncreaseInLocation", lat, lon));
        sub.subConcentrationInc(district, lat, lon);
        Subscription s = new Subscription(com);
        s.sendSubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "ConcentrationIncreaseInLocation");
        return s.receiveSubscriptionRes();
    }

    //Método que subscreve a notificação de diminuição da concentração de pessoas numa localização de um certo distrito
    public boolean subConcentrationDec(String district, int lat, int lon) throws DistrictLimitException {
        this.addSubscription(new SubscriptionObj(district, "ConcentrationDecreaseInLocation", lat, lon));
        sub.subConcentrationDec(district, lat, lon);
        Subscription s = new Subscription(com);
        s.sendSubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "ConcentrationDecreaseInLocation");
        return s.receiveSubscriptionRes();
    }

    //Método que subscreve a notificação de quando deixa de haver pessoas numa certa localização de um certo distrito
    public boolean subNobodyInLocation(String district, int lat, int lon) throws DistrictLimitException {
        this.addSubscription(new SubscriptionObj(district, "NobodyInLocation", lat, lon));
        sub.subNobodyInLocation(district, lat, lon);
        Subscription s = new Subscription(com);
        s.sendSubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "NobodyInLocation");
        return s.receiveSubscriptionRes();
    }

    //Método que subscreve a notificação de occorrência de mais um infetado num dado distrito
    public boolean subInfectionsIncrease(String district) throws DistrictLimitException {
        this.addSubscription(new SubscriptionObj(district, "InfectionsIncrease"));
        sub.subInfectionsIncrease(district);
        Subscription s = new Subscription(com);
        s.sendSubscriptionReq(district + "_" + "InfectionsIncrease");
        return s.receiveSubscriptionRes();
    }

    public boolean unsubConcentrationInc(String district, int lat, int lon) {
        if (sub.unsubConcentrationInc(district, lat, lon)) {
            this.removeSubscription(new SubscriptionObj(district, "ConcentrationIncreaseInLocation", lat, lon));
            Subscription s = new Subscription(com);
            s.sendUnsubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "ConcentrationIncreaseInLocation");
            return s.receiveUnsubscriptioneRes();
        }
        return false;
    }

    public boolean unsubConcentrationDec(String district, int lat, int lon) {
        if (sub.unsubConcentrationDec(district, lat, lon)) {
            this.removeSubscription(new SubscriptionObj(district, "ConcentrationDecreaseInLocation", lat, lon));
            Subscription s = new Subscription(com);
            s.sendUnsubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "ConcentrationDecreaseInLocation");
            return s.receiveUnsubscriptioneRes();
        }
        return false;
    }

    public boolean unsubNobodyInLocation(String district, int lat, int lon) {
        if (sub.unsubNobodyInLocation(district, lat, lon)) {
            this.removeSubscription(new SubscriptionObj(district, "NobodyInLocation", lat, lon));
            Subscription s = new Subscription(com);
            s.sendUnsubscriptionReq(district + "(" + lat + "," + lon + ")" + "_" + "NobodyInLocation");
            return s.receiveUnsubscriptioneRes();
        }
        return false;
    }

    public boolean unsubInfectionsIncrease(String district) {
        if (sub.unsubInfectionsIncrease(district)) {
            this.removeSubscription(new SubscriptionObj(district, "InfectionsIncrease"));
            Subscription s = new Subscription(com);
            s.sendUnsubscriptionReq(district + "_" + "NobodyInLocation");
            return s.receiveUnsubscriptioneRes();
        }
        return false;
    }

    private List<String> subscriptions() {
        Subscription s = new Subscription(com);
        s.sendSubscriptionsReq();
        return s.receiveSubscriptionsReq();
    }


    public String receiveNotification() {
        return sub.receive();
    }

    public void addSubscription(String s) throws DistrictLimitException {
        subs.add(s);
    }

    public void addSubscription(SubscriptionObj s) throws DistrictLimitException {
        subs.add(s);
    }

    public void removeSubscription(String s) {
        subs.remove(s);
    }

    public void removeSubscription(SubscriptionObj s) {
        subs.remove(s);
    }

    public boolean updateSubscriptions() {
        try {
            List<String> l = this.subscriptions();
            for (String s : l) {
                sub.subscribe(s);
                this.addSubscription(s);
            }
            return true;
        } catch (DistrictLimitException e) {
            return false;
        }
    }

    public Map<String, List<SubscriptionObj>> getSubscriptions() {
        return this.subs.getSubs();
    }


}
