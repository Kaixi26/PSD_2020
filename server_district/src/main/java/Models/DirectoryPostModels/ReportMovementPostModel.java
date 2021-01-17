package Models.DirectoryPostModels;

import Models.Location;
import com.google.gson.annotations.SerializedName;

public class ReportMovementPostModel {
    @SerializedName("name")
    private final String districtName;
    @SerializedName("longitude")
    private final int longitude;
    @SerializedName("latitude")
    private final int latitude;
    @SerializedName("maxUsers")
    private final int numberOfUserInLocation;

    public ReportMovementPostModel(String districtName, Location location, int numberOfUserInLocation) {
        this.districtName = districtName;
        this.longitude = location.getLongitude();
        this.latitude = location.getLatitude();
        this.numberOfUserInLocation = numberOfUserInLocation;
    }
}
