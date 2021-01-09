package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;
import com.google.gson.annotations.SerializedName;

public class  AnnounceDistrictServerRequest extends RequestProtocol {
    @SerializedName("districtName")
    private final String DistrictName;

    @SerializedName("server_ip")
    private final String ServerIP;

    @SerializedName("server_port")
    private final int ServerPort;


    public AnnounceDistrictServerRequest(String districtName, String serverIP, int serverPort) {
        super(CommunicationType.AnnounceDistrictServer);
        this.DistrictName = districtName;
        this.ServerIP = serverIP;
        this.ServerPort = serverPort;
    }
}