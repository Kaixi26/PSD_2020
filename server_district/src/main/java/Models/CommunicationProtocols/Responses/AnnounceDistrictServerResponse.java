package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationType;

public class AnnounceDistrictServerResponse extends ResponseProtocol {
    public AnnounceDistrictServerResponse(int statusCode) {
        super(CommunicationType.AnnounceDistrictServer, statusCode);
    }
}
