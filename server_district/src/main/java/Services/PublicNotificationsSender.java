package Services;

import Auxiliar.DistrictServerConfigurations;
import Models.Location;
import com.google.gson.Gson;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.concurrent.locks.ReentrantLock;

public class PublicNotificationsSender {
    private ReentrantLock locker;
    private final String districtName;
    private Gson gson;
    private ZMQ.Socket socketPUB;

    public PublicNotificationsSender(DistrictServerConfigurations configurations, Gson gson) {
        this.locker = new ReentrantLock();
        this.districtName = configurations.getDistrictName();
        this.gson = gson;

        final ZContext context = new ZContext();
        this.socketPUB = context.createSocket(SocketType.PUB);
        this.socketPUB.bind("tcp://" + configurations.getPublicNotificationsIP() + ":" + configurations.getPublicNotificationsPort());
    }

    public void concentrationIncreaseInLocation(Location location) {
        this.locker.lock();
        System.out.println("Public Notifications: Concetration Increase");
        this.locker.unlock();
    }

    public void concentrationDecreaseInLocation(Location location) {
        this.locker.lock();
        System.out.println("Public Notifications: Concetration Decrease");
        this.locker.unlock();
    }

    public void nobodyInLocation(Location location) {
        this.locker.lock();
        System.out.println("Public Notifications: Nobody");
        this.locker.unlock();
    }

    public void infectionsIncrease() {
        this.locker.lock();
        System.out.println("Public Notifications: Infections");
        this.locker.unlock();
    }
}
