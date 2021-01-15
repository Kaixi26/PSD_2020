package Controller;

import Model.Communications.Communication;
import View.View;

import javax.imageio.IIOException;
import java.io.BufferedReader;
import java.io.Console;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ConnectException;
import java.util.Scanner;

public class Controller {
    private Communication model;
    private View view;

    public Controller() throws IOException {
        this.model = new Communication();
        this.view = new View();
    }

    public void init(){
        view.init();
        try {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

        boolean alive = true;
        String[] input;
        while(alive){
            switch ((input = in.readLine().split(" "))[0]){
                case "login":
                    if(input.length > 1) {
                        view.showInvalidInput(input[1]);
                        break;
                    }
                    Runtime.getRuntime().exec("clear");
                    view.showLogin();
                    String username = view.getUsername();
                    if (!model.authenticate(username, view.getPassword()))
                        System.out.println("Error in login"); //TODO view for this
                    else {
                        System.out.println("Login successful"); //TODO view for this
                        new LoggedController(model,view,username).init();
                    }
                    break;
                case "register":
                    if(input.length > 1) {
                        view.showInvalidInput(input[1]);
                        break;
                    }
                    Runtime.getRuntime().exec("clear");
                    view.showRegister();
                    if(!model.register(view.getUsername(), view.getPassword(), view.getDomicile()))
                        System.out.println("Error in register"); //TODO view for this
                    else System.out.println("Register successful"); //TODO view for this
                    break;
                case "help":
                    if(input.length > 1) {
                        view.showInvalidInput(input[1]);
                        break;
                    }
                    view.showMainHelp();
                    break;
                case "exit":
                    if(input.length > 1) {
                        view.showInvalidInput(input[1]);
                        break;
                    }
                    alive = false;
                    break;
                default:
                    view.showInvalidInput();
            }
        }

        } catch (ConnectException e){
            System.out.println();
        }
        catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
    }
}
