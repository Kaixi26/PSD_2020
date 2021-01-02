package Models.PublicNotifications;

import Models.Location;
import com.google.gson.annotations.SerializedName;

public class NobodyInLocation extends NotificationProtocol {
    @SerializedName("location")
    private Location location;

    public NobodyInLocation(String districtName, Location location) {
        super(districtName, NotificationType.NobodyInLocation);
        this.location = location;
    }
}
