package Business;

import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

public class DistrictMapCellManager {
    private ReentrantLock locker;
    private Set<String> presentClients;

    public DistrictMapCellManager() {
        this.locker = new ReentrantLock();
        this.presentClients = new HashSet<>();
    }

    public void lockMapCell() {
        this.locker.lock();
    }

    public void unlockMapCell() {
        this.locker.unlock();
    }

    public void addClient(@NotNull String username) {
        this.presentClients.add(username);
    }

    public void removeClient(@NotNull String username) {
        this.presentClients.remove(username);
    }

    public Set<String> getClientsPresent(){
        Set<String> result = new HashSet<>();
        this.presentClients.forEach(username -> result.add(username));
        return result;
    }

    public int getNumberOfClientsPresent(){
        return this.presentClients.size();
    }

    public boolean isEmpty(){
        return this.presentClients.isEmpty();
    }
}
