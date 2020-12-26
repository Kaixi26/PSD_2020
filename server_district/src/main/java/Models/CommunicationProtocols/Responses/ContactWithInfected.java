package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationProtocol;

import java.util.Set;

public class ContactWithInfected extends CommunicationProtocol {
    private final Set<String> usernames;

    public ContactWithInfected(Set<String> usernames) {
        super(ResponseTypes.ContactWithInfected);
        this.usernames = usernames;
    }
}
