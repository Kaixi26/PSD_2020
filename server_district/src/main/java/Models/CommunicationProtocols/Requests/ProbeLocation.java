package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationProtocol;
import Models.Location;

public class ProbeLocation extends CommunicationProtocol {
    private final Location location;

    public ProbeLocation(int latitude, int longitude) {
        super(RequestTypes.ProbeLocation);
        this.location = new Location(latitude,longitude);
    }

    public Location getLocation() {
        return this.location;
    }
}