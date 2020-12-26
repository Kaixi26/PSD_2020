package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationProtocol;
import Models.Location;

public class NotifyLocation extends CommunicationProtocol {
    private final String user;
    private final Location location;

    public NotifyLocation(String user, int latitude, int longitude) {
        super(RequestTypes.NotifyLocation);
        this.user = user;
        this.location = new Location(latitude,longitude);
    }

    public String getName() {
        return this.user;
    }

    public Location getLocation() {
        return this.location;
    }
}
