package Business;

import Models.CommunicationProtocols.Requests.NotifyInfectionRequest;
import Models.CommunicationProtocols.Requests.NotifyLocationRequest;
import Models.CommunicationProtocols.Requests.ProbeLocationRequest;
import Models.CommunicationProtocols.Responses.NotifyInfectionResponse;
import Models.CommunicationProtocols.Responses.NotifyLocationResponse;
import Models.CommunicationProtocols.Responses.ProbeLocationResponse;
import Models.Location;
import Services.ServiceResult;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;

import java.util.Set;

public class MasterManager {
    private DistrictMapManager districtMapManager;
    private ClientsLocationManager clientsLocationManager;
    private ClientsContactsManager clientsContactsManager;

    public MasterManager(int districtDimension) {
        this.districtMapManager = new DistrictMapManager(districtDimension);
        this.clientsLocationManager = new ClientsLocationManager();
        this.clientsContactsManager = new ClientsContactsManager();
    }

    public NotifyLocationResponse moveClientToLocation(@NotNull NotifyLocationRequest requestModel) {
        if(requestModel.getUsername() == null || requestModel.getLocation().getLatitude() < 0 || requestModel.getLocation().getLongitude() < 0) {
            return new NotifyLocationResponse(HttpStatus.BAD_REQUEST.value());
        }

        Location source = null;
        if(!this.clientsLocationManager.isFirstTime(requestModel.getUsername())) {
            source = this.clientsLocationManager.getClientLocation(requestModel.getUsername());

            if(source.equals(requestModel.getLocation())) {
                return new NotifyLocationResponse(HttpStatus.BAD_REQUEST.value());
            }
        }

        ServiceResult<Set<String>> result = this.districtMapManager.moveClientToLocation(requestModel.getUsername(), source, requestModel.getLocation());
        if(result.isSuccess()) {
            this.clientsLocationManager.putClientLocation(requestModel.getUsername(), requestModel.getLocation());
            this.clientsContactsManager.addContact(result.getResult());
            return new NotifyLocationResponse(HttpStatus.OK.value());
        } else {
            return new NotifyLocationResponse(HttpStatus.BAD_REQUEST.value());
        }
    }

    public ProbeLocationResponse getNumberOfClientsInLocation(@NotNull ProbeLocationRequest requestModel) {
        if(requestModel.getLocation().getLatitude() < 0 || requestModel.getLocation().getLongitude() < 0) {
            return new ProbeLocationResponse(HttpStatus.BAD_REQUEST.value(), -1);
        }

        ServiceResult<Integer> result = this.districtMapManager.getNumberOfClientsInLocation(requestModel.getLocation());
        if(result.isSuccess()) {
            return new ProbeLocationResponse(HttpStatus.OK.value(), result.getResult());
        } else {
            return new ProbeLocationResponse(HttpStatus.BAD_REQUEST.value(), -1);
        }
    }

    public NotifyInfectionResponse clientInfected(@NotNull NotifyInfectionRequest requestModel){
        if(requestModel.getUsername() == null) {
            return new NotifyInfectionResponse(HttpStatus.BAD_REQUEST.value(), null);
        }
        Set<String> contacts = this.clientsContactsManager.getAllContacts(requestModel.getUsername());
        Location actualLocation = this.clientsLocationManager.getClientLocation(requestModel.getUsername());
        this.districtMapManager.removeClientFromMap(requestModel.getUsername(), actualLocation);
        this.clientsLocationManager.removeClientLocation(requestModel.getUsername());
        return new NotifyInfectionResponse(HttpStatus.OK.value(), contacts);
    }
}
