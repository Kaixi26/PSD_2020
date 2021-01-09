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
        //TODO limitar a 3 distritos em simultaneo
        //TODO subscribe na front-end para persistencia
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String[] input;
            boolean alive = true;

            Thread contact = new Thread(new InContactReceiver());
            System.out.println("Created Thread");
            contact.start();

            Thread notReceiver = new Thread(new NotificationReceiver());
            System.out.println("Created Thread");
            notReceiver.start();



            while(alive){
                switch ((input = in.readLine().split(" "))[0]){
                    case "probe":
                        int n = model.probe(Integer.parseInt(input[1]),Integer.parseInt(input[2]));
                        if(n != -1)
                            System.out.println("O número de pessoas infetadas nesta localização é: " + n); //TODO view for this
                        else
                            System.out.println("Error retrieving probe request"); //TODO view for this
                        break;
                    case "move":
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
                    case "subscribe":
                        if(input[1] == null)
                            view.showInvalidInput();
                        switch (input[1].toLowerCase()){
                            case "nobodyin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.subNobodyInLocation(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao subscrever");
                                else System.out.println("Subscrito com sucesso");
                                break;
                            case "concentrationincin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.subConcentrationInc(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao subscrever");
                                else System.out.println("Subscrito com sucesso");
                                break;
                            case "concentrationdecin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.subConcentrationDec(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao subscrever");
                                else System.out.println("Subscrito com sucesso");
                                break;
                            case "anotherinfectedin":
                                if(input[2] == null)
                                    view.showInvalidInput();
                                else if(!model.subInfectionsIncrease(input[2]))
                                    System.out.println("Erro ao subscrever");
                                else System.out.println("Subscrito com sucesso");
                                break;
                            default:
                                view.showInvalidInput();
                        }
                        break;
                    case "unsubscribe":
                        if(input[1] == null)
                            view.showInvalidInput();
                        switch (input[1].toLowerCase()){
                            case "nobodyin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.unsubNobodyInLocation(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao eliminar a subscrição");
                                else System.out.println("Deubscrito com sucesso");
                                break;
                            case "concentrationincin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.unsubConcentrationInc(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao desubscrever");
                                else System.out.println("Desubscrito com sucesso");
                                break;
                            case "concentrationdecin":
                                if(input[2] == null || input[3] == null || input[4] == null)
                                    view.showInvalidInput();
                                else if(!model.unsubConcentrationDec(input[2],Integer.parseInt(input[3]),Integer.parseInt(input[4])))
                                    System.out.println("Erro ao desubscrever");
                                else System.out.println("Desubscrito com sucesso");
                                break;
                            case "anotherinfectedin":
                                if(input[2] == null)
                                    view.showInvalidInput();
                                else if(!model.unsubInfectionsIncrease(input[2]))
                                    System.out.println("Erro ao desubscrever");
                                else System.out.println("Desubscrito com sucesso");
                                break;
                            default:
                                view.showInvalidInput();
                        }
                        break;
                    case "logoff":
                        alive = false;
                        contact.interrupt();
                        contact.join();
                        notReceiver.interrupt();
                        notReceiver.join();
                        model.deAuthenticate();
                        model.clearQueue();
                        model.restartSubscriber();
                        System.out.println("Loggedout");
                        break;
                    default:
                        System.out.println(input[0]);
                        view.showInvalidInput();
                }
            }

        } catch (Exception ex){
            ex.getStackTrace();
        }
    }

    public class InContactReceiver implements Runnable { //TODO try to put this inside the model
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

    public class NotificationReceiver implements Runnable {
        @Override
        public void run() {
            try{
                System.out.println("created");
                //noinspection InfiniteLoopStatement
                while(!currentThread().isInterrupted()){
                    System.out.println("alive");
                    System.out.println("Notificação: " + model.receiveNotification()); //TODO view for this
                }
                throw new InterruptedException();
            } catch (InterruptedException e){
                System.out.println("DEAD");
                //Do nothing (let thread die)
            }
        }
    }
}
