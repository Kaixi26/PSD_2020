package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationType;

public class NotifyLocationResponse extends ResponseProtocol {

    public NotifyLocationResponse(int statusCode) {
        super(CommunicationType.NotifyLocation, statusCode);
    }
}
