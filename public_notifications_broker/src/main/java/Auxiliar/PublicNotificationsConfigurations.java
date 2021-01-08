package Auxiliar;

public class PublicNotificationsConfigurations {
    private final String PublicNotificationsIP_ServersDistrict;
    private final int PublicNotificationsPort_ServersDistrict;
    private final String PublicNotificationsIP_Clients;
    private final int PublicNotificationsPort_Clients;

    public PublicNotificationsConfigurations(String publicNotificationsIP_ServersDistrict, int publicNotificationsPort_ServersDistrict, String publicNotificationsIP_Clients, int publicNotificationsPort_Clients) {
        this.PublicNotificationsIP_ServersDistrict = publicNotificationsIP_ServersDistrict;
        this.PublicNotificationsPort_ServersDistrict = publicNotificationsPort_ServersDistrict;
        this.PublicNotificationsIP_Clients = publicNotificationsIP_Clients;
        this.PublicNotificationsPort_Clients = publicNotificationsPort_Clients;
    }

    public String getPublicNotificationsIP_ServersDistrict() {
        return this.PublicNotificationsIP_ServersDistrict;
    }

    public int getPublicNotificationsPort_ServersDistrict() {
        return this.PublicNotificationsPort_ServersDistrict;
    }

    public String getPublicNotificationsIP_Clients() {
        return this.PublicNotificationsIP_Clients;
    }

    public int getPublicNotificationsPort_Clients() {
        return this.PublicNotificationsPort_Clients;
    }
}
