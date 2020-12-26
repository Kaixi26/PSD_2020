package Business;

import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class DistrictMapCellManager {
    private Set<String> presentClients;

    public DistrictMapCellManager() {
        this.presentClients = new HashSet<>();
    }

    public synchronized Set<String> addClient(@NotNull String username) {
        Set<String> clientsInLocation = new HashSet<>();
        this.presentClients.add(username);
        this.presentClients.forEach(client -> clientsInLocation.add(client));
        return clientsInLocation;
    }

    public synchronized void removeClient(@NotNull String username) {
        this.presentClients.remove(username);
    }

    public synchronized int getNumberOfClientsPresent(){
        return this.presentClients.size();
    }
}
