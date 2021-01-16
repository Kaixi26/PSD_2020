package Controller;

import Model.Communications.Communication;
import View.View;
import View.LoggedControllerView;
import Model.Communications.local.DistrictLimitException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;


public class LoggedController {

    private Communication model;
    private View mainview;
    private LoggedControllerView view;
    private String username;
    private List<String> subs; //List of the districts the user has subscribed, max==3

    public LoggedController(Communication model,View view, String username) throws IOException {
        this.model = model;
        this.mainview = view;
        this.view = view.lv;
        this.username = username;
    }

    public void init(){
        view.init(username);
        model.updateSubscriptions();
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String[] input;
            boolean alive = true;

            Thread contact = new Thread(new InContactReceiver());
            contact.start();

            Thread notReceiver = new Thread(new NotificationReceiver());
            notReceiver.start();



            while(alive){
                switch ((input = in.readLine().split(" "))[0]){
                    case "probe":
                        if(input.length != 3) {
                            mainview.showInvalidInput();
                            break;
                        }
                        int n = model.probe(Integer.parseInt(input[1]),Integer.parseInt(input[2]));
                        if(n != -1)
                            view.showProbeView(n,true);
                        else
                            view.showProbeView(-1,false);
                        break;
                    case "move":
                        if(input.length != 3 ) {
                            mainview.showInvalidInput();
                            break;
                        }
                        if(!model.position(Integer.parseInt(input[1]),Integer.parseInt(input[2])))
                            view.showMoveView(false);
                        else  view.showMoveView(true);
                        break;
                    case "sick":
                        Runtime.getRuntime().exec("clear");
                        if(input.length != 1) {
                            mainview.showInvalidInput();
                            break;
                        }
                        if(!model.sick())
                            view.showSickView(false);
                        else {
                            view.showSickView(true);
                            alive = false;
                            contact.interrupt();
                            notReceiver.interrupt();
                            new Controller().init();
                            break;
                        }
                        break;
                    case "subscriptions":
                        System.out.println(model.getSubscriptions());
                        break;
                    case "subscribe":
                        if(input.length <= 1)
                            mainview.showInvalidInput();
                        try{
                            switch (input[1].toLowerCase()){
                                case "nobodyin":
                                    if(input.length != 5) {
                                        mainview.showInvalidInput();
                                        break;
                                    }
                                    else {
                                        view.showSubscribeView(model.subNobodyInLocation(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                    }
                                    break;
                                case "concentrationincin":
                                    if(input.length != 5) {
                                        mainview.showInvalidInput();
                                        break;
                                    }
                                    else {
                                        view.showSubscribeView(model.subConcentrationInc(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                    }
                                    break;
                                case "concentrationdecin":
                                    if(input.length != 5) {
                                        mainview.showInvalidInput();
                                        break;
                                    }
                                    else {
                                        view.showSubscribeView(model.subConcentrationDec(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                    }
                                    break;
                                case "anotherinfectedin":
                                    if(input.length != 3) {
                                        mainview.showInvalidInput();
                                        break;
                                    }
                                    else {
                                        view.showSubscribeView(model.subInfectionsIncrease(input[2]));
                                    }
                                    break;
                                default:
                                    mainview.showInvalidInput();
                                    break;
                            }
                        } catch (DistrictLimitException e){
                            view.showMaxSubscribedDistrictsWarning();
                        }
                        break;
                    case "unsubscribe":
                        if(input.length <= 1)
                            mainview.showInvalidInput();
                        switch (input[1].toLowerCase()){
                            case "nobodyin":
                                if(input.length != 5) {
                                    mainview.showInvalidInput();
                                    break;
                                }
                                else {
                                    view.showUnsubscribeView(model.unsubNobodyInLocation(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                }
                                break;
                            case "concentrationincin":
                                if(input.length != 5) {
                                    mainview.showInvalidInput();
                                    break;
                                }
                                else view.showUnsubscribeView(model.unsubConcentrationInc(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                break;
                            case "concentrationdecin":
                                if(input.length != 5) {
                                    mainview.showInvalidInput();
                                    break;
                                }
                                else view.showUnsubscribeView(model.unsubConcentrationDec(input[2], Integer.parseInt(input[3]), Integer.parseInt(input[4])));
                                break;
                            case "anotherinfectedin":
                                if(input.length != 3) {
                                    mainview.showInvalidInput();
                                    break;
                                }
                                else view.showUnsubscribeView(model.unsubInfectionsIncrease(input[2]));
                                break;
                            default:
                                mainview.showInvalidInput();
                        }
                        break;
                    case "help":
                        if(input.length > 1) {
                            mainview.showInvalidInput(input[1]);
                            break;
                        }
                        view.help();
                        break;
                    case "logoff":
                        if(input.length > 1) {
                            mainview.showInvalidInput(input[1]);
                            break;
                        }
                        view.showLogOutView();
                        alive = false;
                        contact.interrupt();
                        notReceiver.interrupt();
                        new Controller().init();
                        break;
                    default:
                        mainview.showInvalidInput(input[0]);
                }
            }

        } catch (Exception ex){
            ex.getStackTrace();
        }
    }

    public class InContactReceiver implements Runnable {
        @Override
        public void run() {
            try{
                //noinspection InfiniteLoopStatement
                while(!Thread.interrupted() && model.contact()){
                    view.showContactWarning();
                }
                throw new InterruptedException();
            } catch (Exception e){
                //Do nothing (let thread die)
            }
        }
    }

    public class NotificationReceiver implements Runnable {
        @Override
        public void run() {
            try{
                String not;
                //noinspection InfiniteLoopStatement
                while(!Thread.interrupted() && (not = model.receiveNotification()) != null){
                    view.showNotificationView(not);
                }
                throw new InterruptedException();
            } catch (Exception e){
                //Do nothing (let thread die)
            }
        }
    }
}
