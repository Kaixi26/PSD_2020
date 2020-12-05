import Model.Communications.Authentication;
import Model.Communications.Communication;
import Model.Communications.Registration;

import java.net.Socket;

import static java.lang.Thread.sleep;

public class main {
    public static void main(String[] args) throws Exception {
        Communication com = new Communication();
        com.authenticate("Michael","Scofield");
        com.register("Michael","Scofield","Fox River");
    }
}
