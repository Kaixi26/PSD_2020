package Models.DirectoryPostModels;

import com.google.gson.annotations.SerializedName;

public class ReportNewUserPostModel {
    @SerializedName("name")
    private final String districtName;

    public ReportNewUserPostModel(String districtName) {
        this.districtName = districtName;
    }
}
