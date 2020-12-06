import Model.Communications.Authentication;
import Model.Communications.Communication;
import Model.Communications.Registration;

import java.net.Socket;

import static java.lang.Thread.sleep;

public class teste {
    public static void main(String[] args)  {
        Communication com = new Communication();
        com.probe(2,3);
    }
}
