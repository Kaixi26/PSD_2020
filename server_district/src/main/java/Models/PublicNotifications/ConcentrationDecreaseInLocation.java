package Models.PublicNotifications;

import Models.Location;
import com.google.gson.annotations.SerializedName;

public class ConcentrationDecreaseInLocation extends NotificationProtocol {
    @SerializedName("location")
    private Location location;

    public ConcentrationDecreaseInLocation(String districtName, Location location) {
        super(districtName, NotificationType.ConcentrationDecreaseInLocation);
        this.location = location;
    }
}
