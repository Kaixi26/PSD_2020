package Models.CommunicationProtocols;

public abstract class CommunicationProtocol {
    private final String version = "1.0.0";
    private CommunicationType communicationType;

    public CommunicationProtocol(CommunicationType communicationType) {
        this.communicationType = communicationType;
    }
}
