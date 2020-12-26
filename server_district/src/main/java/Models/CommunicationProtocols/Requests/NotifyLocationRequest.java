package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import Models.Location;
import com.google.gson.annotations.SerializedName;

public class NotifyLocationRequest extends RequestProtocol {
    @SerializedName("username")
    private final String Username;
    @SerializedName("location")
    private final Location location;

    public NotifyLocationRequest(String username, int latitude, int longitude) {
        super(CommunicationType.NotifyLocation);
        this.Username = username;
        this.location = new Location(latitude, longitude);
    }

    public String getUsername() {
        return this.Username;
    }

    public Location getLocation() {
        return this.location;
    }
}
