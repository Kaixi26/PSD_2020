package helloworld.resources;

import helloworld.api.District;
import helloworld.api.Location;
import helloworld.api.User;
import org.checkerframework.checker.units.qual.A;

import java.util.*;
import java.util.stream.Collectors;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/location")
@Produces(MediaType.APPLICATION_JSON)
public class LocationResource {
    public Map<String, List<Location>> locations = new HashMap<>();

    public LocationResource() {

    }

    @GET
    @Path("/top5")
    public List<Location> top5Locations(@QueryParam("district") String district) {
        List<Location> top5 = locations.get(district);

        return top5.stream().limit(5).sorted(Comparator.comparing(Location::getCurrentUsers).reversed()).collect(Collectors.toList());

    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    public Response add(District dist) {
        synchronized (this) {
            System.out.println("Nova Entrada");

            Location loc = new Location(dist.location.latitude, dist.location.longitude, dist.location.currentUsers);

            if (locations.containsKey(dist.district)) {

                List<Location> l2 = locations.get(dist.district);

                for(Location l : l2) {
                    if (l.latitude == loc.getLatitude() && l.longitude == loc.getLongitude()) {
                        if (l.getCurrentUsers() <= loc.getCurrentUsers()) {
                            l.currentUsers = dist.location.currentUsers;
                            System.out.println("ADICIONEI DE NOVO");
                            System.out.println(l2.toString());
                        }
                    } else {
                        l2.add(loc);
                        locations.put(dist.district, l2);
                        System.out.println("ADICIONEI SO A LOCALIZAÇAO");
                        System.out.println(l2.toString());
                    }
                }

            }
            else {
                List<Location> l1 = new ArrayList<>();
                l1.add(loc);
                locations.put(dist.district, l1);
                System.out.println("ADICIONEI TUDO");
                System.out.println(l1.toString());

            }
            System.out.println(locations.keySet());
            return Response.ok().build();
        }
    }

}