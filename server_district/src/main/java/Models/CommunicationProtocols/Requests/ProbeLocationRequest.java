package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import Models.Location;
import com.google.gson.annotations.SerializedName;

public class ProbeLocationRequest extends RequestProtocol {
    @SerializedName("location")
    private final Location location;

    public ProbeLocationRequest(int latitude, int longitude) {
        super(CommunicationType.ProbeLocation);
        this.location = new Location(latitude, longitude);
    }

    public Location getLocation() {
        return this.location;
    }
}
