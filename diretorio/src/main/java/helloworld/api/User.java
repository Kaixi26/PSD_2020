package helloworld.api;

import com.fasterxml.jackson.annotation.*;

public class User {
    public String name;

    @JsonCreator
    public User(@JsonProperty("name") String name) {
        this.name= name;
    }


    @Override
    public String toString() {
        return "User{" +
                "name='" + name + '\'' +
                '}';
    }
}

