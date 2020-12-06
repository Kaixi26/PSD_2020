package View;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class View {

    public View(){
    }

    public void init(){
        System.out.println(
                "##########################################################\n"
             +  "##                         App                          ##\n"
             +  "##########################################################\n"
             +  "\n\n"
             +  "Welcome!\n"
        );
    }

    public void showInvalidInput(){
        System.out.println("Invalid Input!");
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
}
