package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import Models.CommunicationProtocols.CommunitationProtocol;
import com.google.gson.annotations.SerializedName;

public abstract class RequestProtocol extends CommunitationProtocol {
    @SerializedName("request_type")
    private final CommunicationType RequestType;

    public RequestProtocol(CommunicationType requestType) {
        this.RequestType = requestType;
    }
}
