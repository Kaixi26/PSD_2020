package Business;

import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class ClientsContacts {
    private Set<String> contacts;

    public ClientsContacts() {
        this.contacts = new HashSet<>();
    }

    public synchronized void addContact(@NotNull String contact) {
        this.contacts.add(contact);
    }

    public synchronized Set<String> getContacts() {
        Set<String> result = new HashSet<>();
        this.contacts.forEach(username -> result.add(username));
        return result;
    }
}
