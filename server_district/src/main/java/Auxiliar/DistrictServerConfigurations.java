package Auxiliar;

public class DistrictServerConfigurations {
    private final String DistrictName;
    private final String FrontendIP;
    private final int FrontendPort;
    private final String DirectoryIP;
    private final int DirectoryPort;
    private final String PublicNotificationsIP;
    private final int PublicNotificationsPort;
    private final int DistrictDimension;
    private final String DistrictServerIP;
    private final int DistrictServerPort;

    public DistrictServerConfigurations(String districtName, String frontendIP, int frontendPort,
                                        String directoryIP, int directoryPort,
                                        String publicNotificationsIP, int publicNotificationsPort,
                                        int districtDimension,
                                        String districtServerIP, int districtServerPort) {
        this.DistrictName = districtName;
        this.FrontendIP = frontendIP;
        this.FrontendPort = frontendPort;
        this.DirectoryIP = directoryIP;
        this.DirectoryPort = directoryPort;
        this.PublicNotificationsIP = publicNotificationsIP;
        this.PublicNotificationsPort = publicNotificationsPort;
        this.DistrictDimension = districtDimension;
        this.DistrictServerIP = districtServerIP;
        this.DistrictServerPort = districtServerPort;
    }

    public String getDistrictName() {
        return DistrictName;
    }

    public String getFrontendIP() {
        return FrontendIP;
    }

    public int getFrontendPort() {
        return FrontendPort;
    }

    public String getDirectoryIP() {
        return DirectoryIP;
    }

    public int getDirectoryPort() {
        return DirectoryPort;
    }

    public String getPublicNotificationsIP() {
        return PublicNotificationsIP;
    }

    public int getPublicNotificationsPort() {
        return PublicNotificationsPort;
    }

    public int getDistrictDimension() {
        return DistrictDimension;
    }

    public String getDistrictServerIP() {
        return DistrictServerIP;
    }

    public int getDistrictServerPort() {
        return DistrictServerPort;
    }
}
