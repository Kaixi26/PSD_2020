package Business;

import Models.Location;
import org.jetbrains.annotations.NotNull;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.HashSet;
import java.util.Set;

public class DistrictMapCellManager {
    private final ZMQ.Socket socketPUSH;
    private final String districtName;
    private final Location location;
    private Set<String> presentClients;
    private static int i = 0;

    public DistrictMapCellManager(ZContext context, String districtName, Location location) {
        this.socketPUSH = context.createSocket(SocketType.PUSH);
        this.socketPUSH.connect("inproc://notifications_workers");
        this.socketPUSH.send( "Ola");
        this.districtName = districtName;
        this.location = location;
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
