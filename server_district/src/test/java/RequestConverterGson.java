import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import com.google.gson.Gson;

public class RequestConverterGson {
    public static void main(String[] args) {
        Gson gson = new Gson();
        System.out.println(gson.toJson(new NotifyInfectionRequest("lazaro")));
    }
}
