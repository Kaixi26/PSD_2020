package Controller;

import Model.Communications.Communication;
import View.View;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class LoggedController {

    private Communication model;
    private View view;

    public LoggedController(Communication model, View view){
        this.model = model;
        this.view = view;
    }

    public void init(){
        //TODO view init(help)
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String[] input;
            boolean alive = true;
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
                    case "logoff":
                        alive = false;
                        break;
                    default:
                        System.out.println(input[0]);
                        view.showInvalidInput();
                }
            }

        } catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
    }
}
