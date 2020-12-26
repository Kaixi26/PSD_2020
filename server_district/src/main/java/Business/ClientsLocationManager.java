package Business;

import Models.Location;

import java.util.HashMap;
import java.util.Map;

public class ClientsLocationManager {
    private Map<String, Location> locationMap;

    public ClientsLocationManager() {
        this.locationMap = new HashMap<>();
    }

    public Location getClientLocation(String username){
        return this.locationMap.get(username);
    }

    public void putClientLocation(String username, Location location){
        this.locationMap.put(username,location);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        for(String user : this.locationMap.keySet()){
            sb.append(user + "    " + this.locationMap.get(user).getLatitude() + " " + this.locationMap.get(user).getLongitude() + " \n");
        }
        return sb.toString();
    }
}
