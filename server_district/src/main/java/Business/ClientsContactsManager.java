package Business;

import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ClientsContactsManager {
    private Map<String, ClientsContacts> contactsMap;

    public ClientsContactsManager() {
        this.contactsMap = new HashMap<>();
    }

    public synchronized void addContact(@NotNull Set<String> contacts){
        for(String username : contacts){

            if(!this.contactsMap.containsKey(username)) {
                this.contactsMap.put(username, new ClientsContacts());
            }

            for(String contact : contacts){
                if(!username.equals(contact)) {
                    this.contactsMap.get(username).addContact(contact);
                }
            }
        }
    }

    public synchronized Set<String> getAllContacts(@NotNull String username) {
        return this.contactsMap.get(username).getContacts();
    }
}
