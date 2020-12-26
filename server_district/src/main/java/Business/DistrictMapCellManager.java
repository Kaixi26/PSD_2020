package Business;

import java.util.HashSet;
import java.util.Set;

public class DistrictMapCellManager {
    private Set<String> presentClients;

    public DistrictMapCellManager() {
        this.presentClients = new HashSet<>();
    }

    public synchronized Set<String> addClient(String username) {
        Set<String> clientsInLocation = new HashSet<>();
        this.presentClients.add(username);
        this.presentClients.forEach(client -> clientsInLocation.add(client));
        return clientsInLocation;
    }

    public synchronized void removeClient(String username) {
        this.presentClients.remove(username);
    }

    public synchronized Set<String> getClientsPresent() {
        Set<String> result = new HashSet<>();
        this.presentClients.forEach(username -> result.add(username));
        return result;
    }

    public synchronized int getNumberOfClientsPresent(){
        return this.presentClients.size();
    }
}
