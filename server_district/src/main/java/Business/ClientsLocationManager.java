package Business;

import Models.Location;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class ClientsLocationManager {
    private Map<String, Location> locationMap;

    public ClientsLocationManager() {
        this.locationMap = new HashMap<>();
    }

    public boolean isFirstTime(@NotNull String username) {
        return !this.locationMap.containsKey(username);
    }

    public Location getClientLocation(@NotNull String username){
        return this.locationMap.get(username);
    }

    public void putClientLocation(@NotNull String username, @NotNull Location location) {
        this.locationMap.put(username,location);
    }

    public void removeClientLocation(@NotNull String username) {
        this.locationMap.remove(username);
    }
}
