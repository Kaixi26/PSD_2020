package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationType;
import Models.CommunicationProtocols.CommunitationProtocol;
import com.google.gson.annotations.SerializedName;

public abstract class ResponseProtocol extends CommunitationProtocol {
    @SerializedName("ResponseType")
    private final CommunicationType ResponseType;
    @SerializedName("code")
    private final int statusCode;

    public ResponseProtocol(CommunicationType responseType, int statusCode) {
        this.ResponseType = responseType;
        this.statusCode = statusCode;
    }

    public int getStatusCode() {
        return this.statusCode;
    }
}
