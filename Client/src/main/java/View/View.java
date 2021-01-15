package View;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class View {

    public LoggedControllerView lv;

    public View(){
        this.lv = new LoggedControllerView();
    }

    public void init(){
        System.out.println(
                "##########################################################\n"
             +  "##                         App                          ##\n"
             +  "##########################################################\n"
             +  "\n\n"
        );
    }

    public void showInvalidInput(){
        System.out.println("Invalid Input!");
    }

    public void showInvalidInput(String input){
        System.out.println("Invalid Input!\n" + input);
    }

    public void showLogin(){
        new LoginView();
    }

    public void showRegister(){
        new RegisterView();
    }

    public String getUsername(){
        System.out.println(
                "Username: "
        );
        String r = "n/a";
        BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));
        try {
            r = in.readLine();
        }catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
        return r;
    }

    public String getPassword(){
        System.out.println(
                "Password: "
        );
        String r = "n/a";
        BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));
        try {
            r = in.readLine();
        }catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
        return r;
    }

    public String getDomicile(){
        System.out.println(
                "Domicile: "
        );
        String r = "n/a";
        BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));
        try {
            r = in.readLine();
        }catch (Exception ex){
            System.out.println(ex.getStackTrace());
        }
        return r;
    }

    public void showMainHelp(){
        System.out.println(
                "commands available:\n" +
                        "\tlogin - to login\n" +
                        "\tregister - to register a new user\n" +
                        "\thelp - for this list\n" +
                        "\texit - exit the application"
        );
    }

}
