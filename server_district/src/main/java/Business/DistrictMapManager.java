package Business;

import Models.Location;
import Services.ServiceResult;

import java.util.Set;

public class DistrictMapManager {
    private final int dimension;
    private DistrictMapCellManager[][] map;

    public DistrictMapManager(int dimension) {
        this.dimension = dimension;
        this.map = new DistrictMapCellManager[this.dimension][this.dimension];

        for(int i = 0 ; i < this.dimension ; i++){
            for(int j = 0 ; i < this.dimension ; j++){
                this.map[i][j] = new DistrictMapCellManager();
            }
        }
    }

    public ServiceResult<Set<String>> moveClientToLocation(String username, Location source, Location destination) {
        boolean success = false;
        Set<String> clientsInLocation = null;
        if(destination.getLatitude() < this.dimension && destination.getLongitude() < this.dimension) {
            success = true;
            this.map[source.getLatitude()][source.getLongitude()].removeClient(username);
            clientsInLocation = this.map[destination.getLatitude()][source.getLongitude()].addClient(username);
        }
        return new ServiceResult<>(success,clientsInLocation);
    }

    public ServiceResult<Integer> getNumberOfClientsInLocation(Location location) {
        boolean success = false;
        int clientsNumberInLocation = 0;
        if(location.getLatitude() < this.dimension && location.getLongitude() < this.dimension) {
            success = true;
            clientsNumberInLocation = this.map[location.getLatitude()][location.getLongitude()].getNumberOfClientsPresent();
        }
        return new ServiceResult<>(success, clientsNumberInLocation);
    }
}
