package Model.Communications;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.ConnectException;
import java.net.Socket;
import java.util.Properties;

public class Communication {
    private String ip;
    private int port;
    private CommunicationHandler com;

    public Communication() throws IOException {
        loadConfigFile();
        Socket socket = new Socket(ip, port);
        this.com = new CommunicationHandler(socket);
    }

    private void loadConfigFile(){
        try (InputStream input = new FileInputStream("src/main/resources/config.properties")) {
            Properties prop = new Properties();
            prop.load(input);
            this.ip = prop.getProperty("Server.frontend.ip");
            this.port = Integer.parseInt(prop.getProperty("Server.frontend.port"));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    //Método que autentica um utilizador, retorna true/false para sucesso/insucesso
    public boolean authenticate(String name,String pass){
        Authentication a = new Authentication(com);
        a.sendAuthenticationReq(name,pass);
        return a.receiveAuthenticationRes();
    }

    //Método que regista um utilizador, retorna true/false para sucesso/insucesso
    public boolean register(String name,String pass,String dom){
        Registration r = new Registration(com);
        r.sendRegistrationReq(name,pass,dom);
        return r.receiveRegistrationRes();
    }

    //Método que atualiza a localização de um utilizador, retorna true/false para sucesso/insucesso
    public boolean position(int lat,int lon){
        Location l = new Location(com);
        l.sendNotifyLocationReq(lat,lon);
        return l.receiveNotifyLocationRes();
    }

    //Método que obtém o número de pessoas numa dada localização do seu distrito
    public int probe(int lat,int lon){
        Location l = new Location(com);
        l.sendProbeLocationReq(lat,lon);
        return l.receiveProbeLocationRes();
    }

    //Método que comunica ao servidor que o utilizador está infetado
    public boolean sick(){
        Infection i = new Infection(com);
        i.sendInfectedReq();
        return i.receiveInfectedRes();
    }

    public boolean contact(){
        return !com.receiveNotification().equals("error");
    }

    public void clearQueue(){
        com.clearNotificationQueue();
    }
}
