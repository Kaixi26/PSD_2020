package Models;

import com.google.gson.annotations.SerializedName;

public class Location {
    @SerializedName("latitude")
    private final int Latitude;
    @SerializedName("longitude")
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

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if((o == null) || (this.getClass() != o.getClass())) {
            return false;
        }
        Location location = (Location) o;
        return this.Latitude == location.Latitude &&
                this.Longitude == location.Longitude;
    }
}
