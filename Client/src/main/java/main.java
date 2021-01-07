import Controller.Controller;

import java.io.IOException;
import java.net.ConnectException;

public class main {
    public static void main(String[] args) {

        Controller c = null;
        try {
            c = new Controller();
            c.init();
        } catch (ConnectException e){
            System.out.println("Error in stabilising connection, is the Server Online? ");
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }

    }
}
