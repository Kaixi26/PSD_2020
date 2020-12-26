package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import Models.Location;
import com.google.gson.annotations.SerializedName;

public class NotifyLocationRequest extends RequestProtocol {
    @SerializedName("username")
    private final String Username;
    @SerializedName("location")
    private final Location location;

    public NotifyLocationRequest(CommunicationType requestType, String username, Location location) {
        super(requestType);
        this.Username = username;
        this.location = location;
    }

    public String getUsername() {
        return this.Username;
    }

    public Location getLocation() {
        return this.location;
    }
}
