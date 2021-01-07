package Controller;

import Model.Communications.Communication;
import View.View;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import static java.lang.Thread.*;

public class LoggedController {

    private Communication model;
    private View view;

    public LoggedController(Communication model,View view) throws IOException {
        this.model = model;
        this.view = view;
    }

    public void init(){
        //TODO view init(help)
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String[] input;
            boolean alive = true;

            model.clearQueue();

            Thread t = new Thread(new NotificationReceiver());
            System.out.println("Created Thread");
            t.start();

            while(alive){
                switch ((input = in.readLine().split(" "))[0]){
                    case "probe":
                        Runtime.getRuntime().exec("clear");
                        int n = model.probe(Integer.parseInt(input[1]),Integer.parseInt(input[2]));
                        if(n != -1)
                            System.out.println("O número de pessoas infetadas nesta localização é: " + n); //TODO view for this
                        else
                            System.out.println("Error retrieving probe request"); //TODO view for this
                        break;
                    case "move":
                        Runtime.getRuntime().exec("clear");
                        if(!model.position(Integer.parseInt(input[1]),Integer.parseInt(input[2])))
                            System.out.println("Error, unable to move"); //TODO view for this
                        else System.out.println("Move successful"); //TODO view for this
                        break;
                    case "sick":
                        Runtime.getRuntime().exec("clear");
                        if(!model.sick())
                            System.out.println("Error, pls try again or call SNS\nDont leave your home"); //TODO view for this
                        else System.out.println("Dont leave home"); //TODO view for this
                        break;
                    case "logoff":
                        alive = false;
                        t.interrupt();
                        t.join();
                        break;
                    default:
                        System.out.println(input.toString());
                        view.showInvalidInput();
                }
            }

        } catch (Exception ex){
            ex.getStackTrace();
        }
    }

    public class NotificationReceiver implements Runnable{
        @Override
        public void run() {
            try{
                System.out.println("created");
                //noinspection InfiniteLoopStatement
                while(!currentThread().isInterrupted()){
                    System.out.println("alive");
                    if (model.contact())
                        System.out.println("Contacto com covid"); //TODO view for this
                }
                throw new InterruptedException();
            } catch (InterruptedException e){
                System.out.println("DEAD");
                //Do nothing (let thread die)
            }
        }
    }
}
