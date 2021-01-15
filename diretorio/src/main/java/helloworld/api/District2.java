package helloworld.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class District2 {
    public String name;
    public int number;

    @JsonCreator
    public District2(@JsonProperty("name") String name,@JsonProperty("number") int number) {
        this.name = name;
        this.number=number;


    }
}