package helloworld.resources;

import helloworld.api.User;
import org.checkerframework.checker.units.qual.A;

import java.util.*;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/district")
@Produces(MediaType.APPLICATION_JSON)
public class DistrictResource {
    private Map<String, ArrayList<User>> users = new HashMap<>();
    private Map<String, ArrayList<User>> infecteds = new HashMap<>();
    private float average_sick_encounter=0;
    private int number_post_encounter=0;

    public DistrictResource() {
        ArrayList<User> users1 = new ArrayList<>();
        User u1 = new User("teste");
        User u2 = new User("teste2");
        User u3 = new User("teste3");
        User u4 = new User("teste4");
        User u5 = new User("teste5");

        users1.add(u1);
        users1.add(u2);
        users1.add(u3);

        ArrayList<User> users2 = new ArrayList<>();
        users2.add(u4);
        users2.add(u5);

        ArrayList<User> users3 = new ArrayList<>();
        users3.add(u5);

        users.put("Braga",users2);
        users.put("Viana",users1);
        users.put("Porto",users1);
        users.put("Lisboa",users1);
        users.put("Guima",users1);
        users.put("Faro",users3);

        infecteds.put("Porto", users2);
        infecteds.put("Braga", users3);
        infecteds.put("Viana", users1);
        infecteds.put("Faro", users1);


    }

    @GET
    public ArrayList<User> usersDistrict(@QueryParam("district") Optional<String> district) {
        if(!district.isPresent()) {
            ArrayList<User> usersAux = new ArrayList<>();
            for(ArrayList<User> aux: users.values()){
                usersAux.addAll(aux);
            }
            return usersAux;
        }

        if(users.get(district.get()) == null)
            return new ArrayList<>();
        return users.get(district.get());
    }

    @GET
    @Path("/number")
    public int getNumberUsers(@QueryParam("district") Optional<String> district) {
        if(!district.isPresent()) {
            ArrayList<User> usersAux = new ArrayList<>();
            for(ArrayList<User> aux: users.values()){
                usersAux.addAll(aux);
            }
            return usersAux.size();
        }

        if(users.get(district.get()) == null)
            return new ArrayList<>().size();
        return users.get(district.get()).size();
    }

    @GET
    @Path("/infected")
    public ArrayList<User> infectedDistrict(@QueryParam("district") Optional<String> district) {
        if(!district.isPresent()) {
            ArrayList<User> infectedsAux = new ArrayList<>();
            for(ArrayList<User> aux: infecteds.values()){
                infectedsAux.addAll(aux);
            }
            return infectedsAux;
        }

        if(infecteds.get(district.get()) == null)
            return new ArrayList<>();
        return infecteds.get(district.get());
    }

    @GET
    @Path("/numberinfected")
    public int getNumberInfected(@QueryParam("district") Optional<String> district) {
        if (!district.isPresent()) {
            ArrayList<User> usersAux = new ArrayList<>();
            for (ArrayList<User> aux : infecteds.values()) {
                usersAux.addAll(aux);
            }
            return usersAux.size();
        }

        if (infecteds.get(district.get()) == null)
            return new ArrayList<>().size();
        return infecteds.get(district.get()).size();
    }
    @GET
    @Path("/top5peopleatsametime")
    public ArrayList<String> getTop5peopleatsametime() {
        ArrayList<String> dist = new ArrayList<>();
        ArrayList<Integer> size = new ArrayList<>();

     users.forEach((key, tab) -> {


        int min =tab.size();
        if( size.size()<5){
            size.add(min);
            dist.add(key);
        }
        else{
            int minimo= Collections.min(size);
            if (minimo< min){
                int index = size.indexOf(minimo);
                size.remove(index);
                dist.remove(index);
                size.add(min);
                dist.add(key);
            }
        }

     });


       return dist;
    }
    @GET
    @Path("/medioencontrosdoentes")
    public float getMedioencontrosdoentes() {
       return average_sick_encounter;
    }


    @GET
    @Path("/top5")
    public ArrayList<String> getTop5() {
       Map<String,Double> racio = new HashMap<>();
       ArrayList<String> top5 = new ArrayList<>();
       for( String dist : users.keySet()){
           Double sizeUsers = (double)users.get(dist).size();
           if(infecteds.containsKey(dist)){
               Double sizeInfects = (double)infecteds.get(dist).size();
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


    @POST
    @Path("/add/encontro/{numero}")
    public Response addUser(@PathParam("numero") int numero) {
        
        
        average_sick_encounter = ( average_sick_encounter*number_post_encounter + numero) / (number_post_encounter+1);
        number_post_encounter++;

            return Response.ok().build();
        

    }


    @POST
    @Path("/add/{district}/{name}")
    public Response addUser(@PathParam("district") String district,@PathParam("name") String name) {
        synchronized (this) {
            if(users.containsKey(district)){
                ArrayList<User> usersAux = users.get(district);
                User newUser = new User(name);
                usersAux.add(newUser);
                users.put(district, usersAux);
            }
            else{
                ArrayList<User> usersAux = new ArrayList<>();
                User newUser = new User(name);
                usersAux.add(newUser);
                users.put(district, usersAux);
            }
            return Response.ok().build();
        }

    }
    @POST
    @Path("/addinfected/{district}/{name}")
    public Response addInfected(@PathParam("district") String district,@PathParam("name") String name) {
        synchronized (this) {
            if(infecteds.containsKey(district)){
                ArrayList<User> infectedsAux = infecteds.get(district);
                User newUser = new User(name);
                infectedsAux.add(newUser);
                infecteds.put(district, infectedsAux);
            }
            else{
                ArrayList<User> infectedsAux = new ArrayList<>();
                User newUser = new User(name);
                infectedsAux.add(newUser);
                infecteds.put(district, infectedsAux);
            }
            return Response.ok().build();
        }
    }


}

