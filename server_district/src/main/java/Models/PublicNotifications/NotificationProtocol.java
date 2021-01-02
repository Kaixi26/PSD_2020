package Models.PublicNotifications;

import com.google.gson.annotations.SerializedName;

public abstract class NotificationProtocol {
    @SerializedName("version")
    private final String Version = "1.0.0";
    @SerializedName("district_name")
    private final String DistrictName;
    @SerializedName("NotificationType")
    private final NotificationType notificationType;

    public NotificationProtocol(String districtName, NotificationType notificationType) {
        this.DistrictName = districtName;
        this.notificationType = notificationType;
    }
}
