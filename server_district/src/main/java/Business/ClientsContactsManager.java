package Business;

import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ClientsContactsManager {
    private Map<String, Set<String>> contactsMap;

    public ClientsContactsManager() {
        this.contactsMap = new HashMap<>();
    }

    public synchronized void addContact(@NotNull Set<String> contacts){
        for(String username : contacts){

            if(!this.contactsMap.containsKey(username)) {
                this.contactsMap.put(username, new HashSet<>());
            }

            for(String contact : contacts){
                if(!username.equals(contact)) {
                    this.contactsMap.get(username).add(contact);
                }
            }
        }
    }

    public synchronized Set<String> getAllContacts(@NotNull String username) {
        Set<String> result = new HashSet<>();
        this.contactsMap.get(username).forEach(user -> result.add(user));
        return result;
    }

    public synchronized void removeClient(@NotNull String username) {
        this.contactsMap.get(username).clear();
        this.contactsMap.remove(username);
    }
}
