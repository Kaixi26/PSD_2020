package Models;

public class Location {
    private final int Latitude;
    private final int Longitude;

    public Location(int latitude, int longitude) {
        this.Latitude = latitude;
        this.Longitude = longitude;
    }

    public int getLatitude() {
        return this.Latitude;
    }

    public int getLongitude() {
        return this.Longitude;
    }
}
