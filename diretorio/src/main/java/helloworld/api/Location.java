package helloworld.api;

public class Location {
    public int latitude;
    public int longitude;
    public int maxUsers;


    public Location(int latitude, int longitude, int maxUsers) {
        this.latitude = latitude;
        this.longitude = longitude;
        this.maxUsers = maxUsers;
    }

    public int getMaxUsers() {
        return maxUsers;
    }

    public int getLatitude() {
        return latitude;
    }

    public int getLongitude() {
        return longitude;
    }

    @Override
    public String toString() {
        return "Location{" +
                "latitude=" + latitude +
                ", longitude=" + longitude +
                ", maxUsers=" + maxUsers +
                '}';
    }
}