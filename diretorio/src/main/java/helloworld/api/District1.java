package helloworld.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class District1 {
    public String name;

    @JsonCreator
    public District1(@JsonProperty("name") String name) {
        this.name = name;

    }
}