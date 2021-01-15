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

        return top5.stream().sorted(Comparator.comparing(Location::getMaxUsers).reversed()).limit(5).collect(Collectors.toList());
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    public Response add(District dist) {
        synchronized (this) {
            System.out.println("Nova Entrada");

            Location loc = new Location(dist.location.latitude, dist.location.longitude, dist.location.maxUsers);

            List<Location> locList = locations.getOrDefault(dist.district, new ArrayList<>());

            int i;
            for(i=0; i<locList.size(); i++)
                if(locList.get(i).latitude == loc.latitude && locList.get(i).longitude == loc.longitude) {
                    if (locList.get(i).getMaxUsers() <= loc.getMaxUsers()) {
                        locList.remove(i);
                        locList.add(loc);
                    }
                    break;
                }
            if(i == locList.size())
                locList.add(loc);

            //System.out.println(locList.toString());
            locations.put(dist.district, locList);

            //System.out.println(locations.keySet());
            return Response.ok().build();
        }
    }
}