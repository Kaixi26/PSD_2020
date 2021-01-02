package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationType;
import com.google.gson.annotations.SerializedName;

public class ProbeLocationResponse extends ResponseProtocol {
    @SerializedName("clientsNumber")
    private int clientsNumber;

    public ProbeLocationResponse(int code, int clientsNumber) {
        super(CommunicationType.ProbeLocation, code);
        this.clientsNumber = clientsNumber;
    }
}