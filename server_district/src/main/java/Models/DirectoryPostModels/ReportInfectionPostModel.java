package Models.DirectoryPostModels;

import com.google.gson.annotations.SerializedName;

public class ReportInfectionPostModel {
    @SerializedName("name")
    private final String districtName;
    @SerializedName("number")
    private final int numberOfContacts;

    public ReportInfectionPostModel(String districtName, int numberOfContacts) {
        this.districtName = districtName;
        this.numberOfContacts = numberOfContacts;
    }
}
