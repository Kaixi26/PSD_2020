package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationProtocol;

public class AnnounceDistrictServer extends CommunicationProtocol {
    private final String districtName;
    private final String ip;
    private final int port;

    public AnnounceDistrictServer(String districtName, String ip, int port) {
        super(RequestTypes.AnnounceDistrictServer);
        this.districtName = districtName;
        this.ip = ip;
        this.port = port;
    }
}
