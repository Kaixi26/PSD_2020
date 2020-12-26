package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import com.google.gson.annotations.SerializedName;

public class NotifyInfectionRequest extends RequestProtocol{
    @SerializedName("username")
    private final String Username;


    public NotifyInfectionRequest(String username) {
        super(CommunicationType.NotifyInfection);
        this.Username = username;
    }

    public String getUsername() {
        return this.Username;
    }
}
