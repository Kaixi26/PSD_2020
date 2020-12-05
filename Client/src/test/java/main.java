import Model.Communication.Authentication;
import Model.Communication.Registration;
import com.google.gson.*;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;

import static java.lang.Thread.sleep;

public class main {
    public static void main(String[] args) throws Exception {
        Authentication auth = new Authentication();
        Registration reg = new Registration();
        auth.sendAuthenticationReq("Michael","Scofield");
        sleep(5000);
        reg.sendRegistrationReq("Michael","Scofield","Fox River");

    }
}
