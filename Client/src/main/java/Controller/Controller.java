package Controller;

import Model.Communications.Communication;
import View.View;

import java.io.BufferedReader;
import java.io.Console;
import java.io.InputStreamReader;

public class Controller {
    private Communication model;
    private View view;

    public Controller(){
        this.model = new Communication();
        this.view = new View();
    }

    public void init(){
        view.init();
        try {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

        boolean alive = true;
        while(alive){
            switch (in.readLine()){
                case "login":
                    Runtime.getRuntime().exec("clear");
                    view.showLogin();
                    if (!model.authenticate(view.getUsername(), view.getPassword()))
                        System.out.println("Error in login"); //TODO view for this
                    else System.out.println("Login successful"); //TODO view for this
                    new LoggedController(model,view).init();
                    break;
                case "register":
                    Runtime.getRuntime().exec("clear");
                    view.showRegister();
                    if(!model.register(view.getUsername(), view.getPassword(), view.getDomicile()))
                        System.out.println("Error in register"); //TODO view for this
                    else System.out.println("Register successful"); //TODO view for this
                    break;
                case "exit":
                    alive = false;
                    break;
                default:
                    view.showInvalidInput();
            }
        }

        } catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
    }
}
