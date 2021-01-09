package Business;

import Auxiliar.Tuple;
import Models.Location;
import Services.ServiceBiResult;
import Services.ServiceResult;
import org.jetbrains.annotations.NotNull;
import org.springframework.core.io.FileSystemResource;

import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

public class DistrictMapManager {
    private final int dimension;
    private Tuple<ReentrantLock,DistrictMapCellManager>[][] map;

    public DistrictMapManager(int dimension) {
        this.dimension = dimension;
        this.map = new Tuple[this.dimension][this.dimension];

        for(int i = 0 ; i < this.dimension ; i++){
            for(int j = 0 ; j < this.dimension ; j++){
                this.map[i][j] = new Tuple(new ReentrantLock(),new DistrictMapCellManager());
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
                Tuple<ReentrantLock,DistrictMapCellManager> sourceDistrictCell = this.map[source.getLatitude()][source.getLongitude()];

                ReentrantLock sourceCellLocker = sourceDistrictCell.getFirst();
                DistrictMapCellManager sourceCellManager = sourceDistrictCell.getSecond();

                sourceCellLocker.lock();
                sourceCellManager.removeClient(username);
                sourceIsEmpty = sourceCellManager.isEmpty();
                sourceCellLocker.unlock();
            }

            Tuple<ReentrantLock,DistrictMapCellManager> destinationDistrictCell = this.map[destination.getLatitude()][destination.getLongitude()];

            ReentrantLock destinationCellLocker = destinationDistrictCell.getFirst();
            DistrictMapCellManager destinationCellManager = destinationDistrictCell.getSecond();

            destinationCellLocker.lock();
            destinationCellManager.addClient(username);
            clientsInLocation = destinationCellManager.getClientsPresent();
            destinationCellLocker.unlock();
        }

        return new ServiceBiResult<>(success, sourceIsEmpty, clientsInLocation);
    }

    public ServiceResult<Integer> getNumberOfClientsInLocation(@NotNull Location location) {
        boolean success = false;
        int clientsNumberInLocation = 0;

        if(location.getLatitude() < this.dimension && location.getLongitude() < this.dimension) {
            success = true;
            Tuple<ReentrantLock,DistrictMapCellManager> districtCell = this.map[location.getLatitude()][location.getLongitude()];

            ReentrantLock cellLocker = districtCell.getFirst();
            DistrictMapCellManager cellManager = districtCell.getSecond();

            cellLocker.lock();
            clientsNumberInLocation = cellManager.getNumberOfClientsPresent();
            cellLocker.unlock();
        }
        return new ServiceResult<>(success, clientsNumberInLocation);
    }

    public boolean removeClientFromMap(@NotNull String username, @NotNull Location actualLocation) {
        Tuple<ReentrantLock,DistrictMapCellManager> districtCell = this.map[actualLocation.getLatitude()][actualLocation.getLongitude()];

        ReentrantLock cellLocker = districtCell.getFirst();
        DistrictMapCellManager cellManager = districtCell.getSecond();

        cellLocker.lock();
        cellManager.removeClient(username);
        boolean locationIsEmpty = cellManager.isEmpty();
        cellLocker.unlock();

        return locationIsEmpty;
    }
}
