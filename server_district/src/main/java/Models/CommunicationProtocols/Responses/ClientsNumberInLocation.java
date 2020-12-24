package Models.CommunicationProtocols.Responses;

import Models.CommunicationProtocols.CommunicationProtocol;

public class ClientsNumberInLocation extends CommunicationProtocol {
    private int clientsNumber;

    public ClientsNumberInLocation(int clientsNumber) {
        super(ResponseTypes.ClientsNumberInLocation);
        this.clientsNumber = clientsNumber;
    }
}
