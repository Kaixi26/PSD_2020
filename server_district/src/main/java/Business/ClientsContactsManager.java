package Business;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

public class ClientsContactsManager {
    private ReentrantLock locker;
    private Map<String, ClientsContacts> contactsMap;

    public ClientsContactsManager() {
        this.contactsMap = new HashMap<>();
    }

    public void addContact(Set<String> contacts){
        for(String username : contacts){

            this.locker.lock();
            if(!this.contactsMap.containsKey(username)) {
                this.contactsMap.put(username, new ClientsContacts());
            }
            this.locker.unlock();

            for(String contact : contacts){
                if(!username.equals(contact)) {
                    this.contactsMap.get(username).addContact(contact);
                }
            }
        }
    }

    public Set<String> getAllContacts(String username) {
        Set<String> result = new HashSet<>();

        this.locker.lock();
        boolean exists = this.contactsMap.containsKey(username);
        this.locker.unlock();

        if(exists){
            result = this.contactsMap.get(username).getContacts();
        }

        return result;
    }
}
