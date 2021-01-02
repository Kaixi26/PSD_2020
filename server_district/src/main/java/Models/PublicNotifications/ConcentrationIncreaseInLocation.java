package Models.PublicNotifications;

import Models.Location;
import com.google.gson.annotations.SerializedName;

public class ConcentrationIncreaseInLocation extends NotificationProtocol {
    @SerializedName("location")
    private Location location;

    public ConcentrationIncreaseInLocation(String districtName, Location location) {
        super(districtName, NotificationType.ConcentrationIncreaseInLocation);
        this.location = location;
    }
}
