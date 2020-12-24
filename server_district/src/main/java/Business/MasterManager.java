package Business;

import Models.Location;
import Services.ServiceResult;

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

    public void moveClientToLocation(String username, Location destination) {
        Location source = this.clientsLocationManager.getClientLocation(username);
        ServiceResult<Set<String>> result = this.districtMapManager.moveClientToLocation(username, source, destination);
        if(result.isSuccess()) {
            this.clientsLocationManager.putClientLocation(username, destination);
            this.clientsContactsManager.addContact(result.getResult());
            /**
             *  Fazer Remove do utilizador que estava na source
             *  Fazer Post do utilizador que esta na destination
             */
        } else {
            /**
             * A localização não é compatível com as dimensões do distrito
             */
        }
    }


    public int getNumberOfClientsInLocation(Location location) {
        Location source = this.clientsLocationManager.getClientLocation(username);
        ServiceResult<Set<String>> result = this.districtMapManager.moveClientToLocation(username, source, destination);
        if(result.isSuccess()) {
            this.clientsLocationManager.putClientLocation(username, destination);
            this.clientsContactsManager.addContact(result.getResult());
            /**
             *  Fazer Remove do utilizador que estava na source
             *  Fazer Post do utilizador que esta na destination
             */
        } else {
            /*
             * A localização não é compatível com as dimensões do distrito
             */
        }
    }

    public Set<String> getAllContacts(String name){
        return this.clientsContactsMapManager.getAllContacts(name);
    }*/
}
