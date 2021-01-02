package Model.Communications;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.Properties;

public class Communication {
    private String ip;
    private int port;
    private Socket socket;

    public Communication(){
        try {
            loadConfigFile();
            this.socket = new Socket(ip,port);
        }
        catch (Exception ex){
            ex.printStackTrace();
        }
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
        Authentication a = new Authentication();
        a.sendAuthenticationReq(name,pass,socket);
        return a.receiveAuthenticationRes(socket);
    }

    //Método que regista um utilizador, retorna true/false para sucesso/insucesso
    public boolean register(String name,String pass,String dom){
        Registration r = new Registration();
        r.sendRegistrationReq(name,pass,dom,socket);
        return r.receiveRegistrationRes(socket);
    }

    //Método que atualiza a localização de um utilizador, retorna true/false para sucesso/insucesso
    public boolean position(int lat,int lon){
        Location l = new Location();
        l.sendNotifyLocationReq(lat,lon,socket);
        return l.receiveNotifyLocationRes(socket);
    }

    //Método que obtém o número de pessoas numa dada localização do seu distrito
    public int probe(int lat,int lon){
        Location l = new Location();
        l.sendProbeLocationReq(lat,lon,socket);
        return l.receiveProbeLocationRes(socket);
    }

    //Método que comunica ao servidor que o utilizador está infetado
    public boolean sick(){
        Infection i = new Infection();
        i.sendInfectedReq(socket);
        return i.receiveInfectedRes(socket);
    }
}
