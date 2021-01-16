package Services;

import Auxiliar.DistrictServerConfigurations;
import Models.DirectoryPostModels.ReportInfectionPostModel;
import Models.DirectoryPostModels.ReportMovementPostModel;
import Models.DirectoryPostModels.ReportNewUserPostModel;
import Models.Location;
import com.google.gson.Gson;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;

import java.io.IOException;
import java.lang.module.Configuration;
import java.net.URI;
import java.net.URISyntaxException;

public class DirectoryPoster {
    private HttpClient httpClient;
    private Gson gson;
    private final String districtName;

    private final String reportInfectionEndPoint = "/district/report";
    private final String reportMovementEndPoint = "/location";
    private final String reportNewUserEndPoint = "/district";

    private final URI reportInfectionURI;
    private final URI reportMovementURI;
    private final URI reportNewUserURI;

    public DirectoryPoster(DistrictServerConfigurations configurations, Gson gson) throws URISyntaxException {
        this.httpClient = new DefaultHttpClient();

        this.districtName = configurations.getDistrictName();

        this.reportInfectionURI = new URI(configurations.getDirectoryDomainURL() + reportInfectionEndPoint);
        this.reportMovementURI = new URI(configurations.getDirectoryDomainURL() + reportMovementEndPoint);
        this.reportNewUserURI = new URI(configurations.getDirectoryDomainURL() + reportNewUserEndPoint);
        
        this.gson = gson;
    }



    public void reportInfection(int numberOfContacts) {
        HttpPost httpPost = new HttpPost(this.reportInfectionURI);
        httpPost.setHeader("Content-type", "application/json");

        String bodyJson = this.gson.toJson(new ReportInfectionPostModel(this.districtName, numberOfContacts));
        System.out.println(bodyJson);

        StringEntity stringEntity = new StringEntity(bodyJson);
        httpPost.getRequestLine();
        httpPost.setEntity(stringEntity);

        HttpResponse response = this.httpClient.execute(httpPost);
        System.out.println("Directory Poster -> Report Infection : " + response.getStatusLine());
    }

    public void reportMovement(Location location, int numberOfUsersInLocation) {
        HttpPost httpPost = new HttpPost(this.reportMovementURI);
        httpPost.setHeader("Content-type", "application/json");

        String bodyJson = this.gson.toJson(new ReportMovementPostModel(this.districtName, location, numberOfUsersInLocation));
        System.out.println(bodyJson);

        StringEntity stringEntity = new StringEntity(this.gson.toJson(bodyJson));
        httpPost.getRequestLine();
        httpPost.setEntity(stringEntity);

        HttpResponse response = this.httpClient.execute(httpPost);

        System.out.println("Directory Poster -> Report Movement : " + response.getStatusLine());
    }

    public void reportNewUser() {
        HttpPost httpPost = new HttpPost(this.reportNewUserURI);
        httpPost.setHeader("Content-type", "application/json");

        String bodyJson = this.gson.toJson(new ReportNewUserPostModel(this.districtName));
        System.out.println(bodyJson);

        StringEntity stringEntity = new StringEntity(bodyJson);
        httpPost.getRequestLine();
        httpPost.setEntity(stringEntity);

        HttpResponse response = this.httpClient.execute(httpPost);

        System.out.println("Directory Poster -> Report New User : " + response.getStatusLine());
    }
}
