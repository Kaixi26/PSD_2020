package helloworld.resources;

import helloworld.api.User;
import org.checkerframework.checker.units.qual.A;

import java.util.*;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/district")
@Produces(MediaType.APPLICATION_JSON)
public class DistrictResource {
    private Map<String, ArrayList<User>> users = new HashMap<>();
    private Map<String, ArrayList<User>> infecteds = new HashMap<>();

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
        users.put("Braga",users1);
        ArrayList<User> users2 = new ArrayList<>();
        users2.add(u4);
        users2.add(u5);
        users.put("Viana",users2);

        infecteds.put("Porto", users1);

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
    @Path("/top5")
    public ArrayList<String> getTop5() {
       return null;
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

