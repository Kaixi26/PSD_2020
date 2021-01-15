package helloworld.resources;

import helloworld.api.District;
import helloworld.api.District1;
import helloworld.api.District2;
import helloworld.api.Location;
import helloworld.api.User;
import org.checkerframework.checker.units.qual.A;

import java.util.*;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/district")
@Produces(MediaType.APPLICATION_JSON)
public class DistrictResource {
    private Map<String, Integer> users = new HashMap<>();
    private Map<String, Integer> infecteds = new HashMap<>();
    private float average_sick_encounter=0;
    private int number_post_encounter=0;

    public DistrictResource() {
    }



    @GET
    public int usersDistrict(@QueryParam("district") String district) {
        int n = 0;

        if(users.containsKey(district)) {
            n = users.get(district);
        }

        return n;
    }

    @GET
    @Path("/infected")
    public int infectedDistrict(@QueryParam("district") String district) {
        int n = 0;

        if(infecteds.containsKey(district)) {
            n = infecteds.get(district);
        }

        return n;
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    public Response add(District1 dist) {
        synchronized (this) {
            if(users.containsKey(dist.name)){
                int number = users.get(dist.name);
                number ++;
                users.put(dist.name, number);
            }
            else{
                users.put(dist.name, 1);
            }
            return Response.ok().build();
        }
    }


    @POST
    @Path("/report")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response addInfected(District2 dist) {
        synchronized (this) {
            if(infecteds.containsKey(dist.name)){
                int number = infecteds.get(dist.name);
                number ++;
                infecteds.put(dist.name, number);
            }
            else{
                infecteds.put(dist.name, 1);
            }
            average_sick_encounter = ( average_sick_encounter*number_post_encounter + dist.number) / (number_post_encounter+1);
            number_post_encounter++;

            return Response.ok().build();
        }
    }
    /*
    @POST
    @Path("/add/{district}")
    public Response addUser(@PathParam("district") String district) {
        synchronized (this) {
            if(users.containsKey(district)){
                int number = users.get(district);
                number ++;
                users.put(district, number);
            }
            else{
                users.put(district, 1);
            }
            return Response.ok().build();
        }
    }


    @POST
    @Path("/addInfect/{district}")
    public Response addInfect(@PathParam("district") String district) {
        synchronized (this) {
            if(infecteds.containsKey(district)){
                int number = infecteds.get(district);
                number ++;
                infecteds.put(district, number);
            }
            else{
                infecteds.put(district, 1);
            }
            return Response.ok().build();
        }
    }
    */

    @GET
    @Path("/top5")
    public List<String>getTop5() {
        Map<String,Double> racio = new HashMap<>();
        ArrayList<String> top5 = new ArrayList<>();
        for( String dist : users.keySet()){
            Double sizeUsers = (double)users.get(dist);
            if(infecteds.containsKey(dist)){
                Double sizeInfects = (double)infecteds.get(dist);
                racio.put(dist,sizeInfects/sizeUsers);
            }
            else {
                racio.put(dist, (double) 0);
            }
        }

        racio.entrySet()
                .stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(5)
                .forEachOrdered(x -> top5.add(x.getKey()));

        return top5;

    }

    @GET
    @Path("/medioencontrosdoentes")
    public float getMedioencontrosdoentes() {
        return average_sick_encounter;
    }

/*
    @POST
    @Path("/add/encontro/{numero}")
    public Response addUser(@PathParam("numero") int numero) {


        average_sick_encounter = ( average_sick_encounter*number_post_encounter + numero) / (number_post_encounter+1);
        number_post_encounter++;

        return Response.ok().build();


    }*/

}

