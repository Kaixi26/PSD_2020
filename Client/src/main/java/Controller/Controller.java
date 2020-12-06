package Controller;

import Model.Communications.Communication;
import View.View;

import java.io.BufferedReader;
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
                    model.authenticate(view.getUsername(),view.getPassword());
                    break;
                case "register":
                    Runtime.getRuntime().exec("clear");
                    view.showRegister();
                    model.register(view.getUsername(), view.getPassword(), view.getDomicile());
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
