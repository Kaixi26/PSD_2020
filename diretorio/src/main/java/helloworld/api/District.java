package helloworld.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.checkerframework.checker.units.qual.A;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class District {
    public String district;
    public Location location;

    @JsonCreator
    public District(@JsonProperty("district") String district,@JsonProperty("longitude") int longitude, @JsonProperty("latitude") int latitude,
                    @JsonProperty("currentUsers") int currentUsers){
        this.district = district;
        Location loc = new Location(latitude,longitude,currentUsers);
        this.location = loc;
    }

    @Override
    public String toString() {
        return "District{" +
                "district='" + district + '\'' +
                ", location=" + location.toString() +
                '}';
    }
}
