package Services;

import Auxiliar.DistrictServerConfigurations;
import Models.Location;
import Models.PublicNotifications.ConcentrationDecreaseInLocation;
import Models.PublicNotifications.ConcentrationIncreaseInLocation;
import Models.PublicNotifications.InfectionsIncrease;
import Models.PublicNotifications.NotificationType;
import com.google.gson.Gson;
import org.jetbrains.annotations.NotNull;
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

    public void concentrationIncreaseInLocation(@NotNull Location location) {
        System.out.println("Public Notifications: Concentration Increase");
        String header = this.districtName + "(" + location.getLatitude() + "," + location.getLongitude() + ")_" + NotificationType.ConcentrationIncreaseInLocation.name();
        String body = this.gson.toJson(new ConcentrationIncreaseInLocation(this.districtName, location));
        String packet = header + " " + body;
        this.locker.lock();
        this.socketPUB.send(packet);
        this.locker.unlock();
    }

    public void concentrationDecreaseInLocation(@NotNull Location location) {
        System.out.println("Public Notifications: Concentration Decrease");
        String header = this.districtName + "(" + location.getLatitude() + "," + location.getLongitude() + ")_" + NotificationType.ConcentrationDecreaseInLocation.name();
        String body = this.gson.toJson(new ConcentrationDecreaseInLocation(this.districtName, location));
        String packet = header + " " + body;
        this.locker.lock();
        this.socketPUB.send(packet);
        this.locker.unlock();
    }

    public void nobodyInLocation(@NotNull Location location) {
        System.out.println("Public Notifications: Nobody");
        String header = this.districtName + "(" + location.getLatitude() + "," + location.getLongitude() + ")_" + NotificationType.NobodyInLocation.name();
        String body = this.gson.toJson(new ConcentrationDecreaseInLocation(this.districtName, location));
        String packet = header + " " + body;
        this.locker.lock();
        this.socketPUB.send(packet);
        this.locker.unlock();
    }

    public void infectionsIncrease() {
        System.out.println("Public Notifications: Infections");
        String header = this.districtName +"_"+ NotificationType.InfectionsIncrease.name();
        String body = this.gson.toJson(new InfectionsIncrease(this.districtName));
        String packet = header + " " + body;
        this.locker.lock();
        this.socketPUB.send(packet);
        this.locker.unlock();
    }
}
