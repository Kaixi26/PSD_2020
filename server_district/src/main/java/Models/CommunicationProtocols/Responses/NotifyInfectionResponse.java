package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationType;
import com.google.gson.annotations.SerializedName;

import java.util.Set;

public class NotifyInfectionResponse extends ResponseProtocol {
    @SerializedName("contacts")
    private Set<String> Contacts;

    public NotifyInfectionResponse(int statusCode, Set<String> contacts) {
        super(CommunicationType.NotifyInfection, statusCode);
        this.Contacts = contacts;
    }
}
