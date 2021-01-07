package Business;

import Models.Location;
import Services.ServiceBiResult;
import Services.ServiceResult;
import org.jetbrains.annotations.NotNull;
import org.springframework.core.io.FileSystemResource;

import java.util.Set;

public class DistrictMapManager {
    private final int dimension;
    private DistrictMapCellManager[][] map;

    public DistrictMapManager(int dimension) {
        this.dimension = dimension;
        this.map = new DistrictMapCellManager[this.dimension][this.dimension];

        for(int i = 0 ; i < this.dimension ; i++){
            for(int j = 0 ; j < this.dimension ; j++){
                this.map[i][j] = new DistrictMapCellManager();
            }
        }
    }

    public ServiceBiResult moveClientToLocation(@NotNull String username, Location source, @NotNull Location destination) {
        boolean success = false;
        boolean sourceIsEmpty = false;
        Set<String> clientsInLocation = null;

        if(destination.getLatitude() < this.dimension && destination.getLongitude() < this.dimension) {
            success = true;
            if(source != null) {
                DistrictMapCellManager sourceDistrictCell = this.map[source.getLatitude()][source.getLongitude()];

                sourceDistrictCell.lockMapCell();
                sourceDistrictCell.removeClient(username);
                sourceIsEmpty = sourceDistrictCell.isEmpty();
                sourceDistrictCell.unlockMapCell();
            }

            DistrictMapCellManager destinationDistrictCell = this.map[destination.getLatitude()][destination.getLongitude()];
            destinationDistrictCell.lockMapCell();
            destinationDistrictCell.addClient(username);
            clientsInLocation = destinationDistrictCell.getClientsPresent();
            destinationDistrictCell.unlockMapCell();
        }

        return new ServiceBiResult<>(success, sourceIsEmpty, clientsInLocation);
    }

    public ServiceResult<Integer> getNumberOfClientsInLocation(@NotNull Location location) {
        boolean success = false;
        int clientsNumberInLocation = 0;

        if(location.getLatitude() < this.dimension && location.getLongitude() < this.dimension) {
            success = true;
            DistrictMapCellManager districtCell = this.map[location.getLatitude()][location.getLongitude()];

            districtCell.lockMapCell();
            clientsNumberInLocation = districtCell.getNumberOfClientsPresent();
            districtCell.unlockMapCell();
        }
        return new ServiceResult<>(success, clientsNumberInLocation);
    }

    public void removeClientFromMap(@NotNull String username, @NotNull Location actualLocation) {
        DistrictMapCellManager districtCell = this.map[actualLocation.getLatitude()][actualLocation.getLongitude()];

        districtCell.lockMapCell();
        districtCell.removeClient(username);
        districtCell.unlockMapCell();
    }
}
