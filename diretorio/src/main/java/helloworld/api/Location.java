package helloworld.api;

public class Location {
    public int latitude;
    public int longitude;
    public int currentUsers;


    public Location(int latitude, int longitude, int currentUsers) {
        this.latitude = latitude;
        this.longitude = longitude;
        this.currentUsers = currentUsers;
    }

    public int getCurrentUsers() {
        return currentUsers;
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
                ", currentUsers=" + currentUsers +
                '}';
    }
}