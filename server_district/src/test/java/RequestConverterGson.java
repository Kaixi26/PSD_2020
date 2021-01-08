import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import Models.Location;
import Models.PublicNotifications.ConcentrationDecreaseInLocation;
import Models.PublicNotifications.ConcentrationIncreaseInLocation;
import Models.PublicNotifications.InfectionsIncrease;
import Models.PublicNotifications.NotificationType;
import com.google.gson.Gson;

import java.time.LocalDate;

public class RequestConverterGson {
    public static void main(String[] args) {
        Gson gson = new Gson();

        String districtName = "Braga";
        Location location = new Location(1,2);

        String header = districtName + "(" + location.getLatitude() + "," + location.getLongitude() + ")_" + NotificationType.ConcentrationIncreaseInLocation.name();
        String body = gson.toJson(new ConcentrationIncreaseInLocation(districtName, location));
        String packet = header + " " + body;

        System.out.println(gson.toJson(packet));
    }
}
