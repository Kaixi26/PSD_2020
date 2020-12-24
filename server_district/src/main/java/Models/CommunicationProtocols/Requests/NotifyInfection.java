package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationProtocol;

public class NotifyInfection extends CommunicationProtocol {
    private final String username;

    public NotifyInfection(String username) {
        super(RequestTypes.NotifyInfection);
        this.username = username;
    }

    public String getUsername() {
        return this.username;
    }
}

