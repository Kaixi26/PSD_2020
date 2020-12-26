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

    @SerializedName("pub_notifications_ip")
    private final String PUBNotificationsIP;

    @SerializedName("pub_notifications_port")
    private final int PUBNotificationsPort;


    public AnnounceDistrictServerRequest(String districtName, String serverIP, int serverPort, String pubNotificationsIP, int pubNotificationsPort) {
        super(CommunicationType.AnnounceDistrictServer);
        this.DistrictName = districtName;
        this.ServerIP = serverIP;
        this.ServerPort = serverPort;
        this.PUBNotificationsIP = pubNotificationsIP;
        this.PUBNotificationsPort = pubNotificationsPort;
    }
}