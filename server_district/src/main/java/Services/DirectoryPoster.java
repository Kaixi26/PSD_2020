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
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.module.Configuration;
import java.net.*;
import java.nio.charset.StandardCharsets;

public class DirectoryPoster {
    private Gson gson;
    private final String districtName;

    private final String reportInfectionEndPoint = "/district/report";
    private final String reportMovementEndPoint = "/location";
    private final String reportNewUserEndPoint = "/district";

    private final URI reportInfectionURI;
    private final URI reportMovementURI;
    private final URI reportNewUserURI;

    public DirectoryPoster(DistrictServerConfigurations configurations, Gson gson) throws URISyntaxException {

        this.districtName = configurations.getDistrictName();

        this.reportInfectionURI = new URI(configurations.getDirectoryDomainURL() + reportInfectionEndPoint);
        this.reportMovementURI = new URI(configurations.getDirectoryDomainURL() + reportMovementEndPoint);
        this.reportNewUserURI = new URI(configurations.getDirectoryDomainURL() + reportNewUserEndPoint);

        this.gson = gson;
    }


    public void reportInfection(int numberOfContacts) {
        HttpClient httpClient = new DefaultHttpClient();
        HttpPost httpPost = new HttpPost(this.reportInfectionURI);

        try {
            String message = this.gson.toJson(new ReportInfectionPostModel(this.districtName, numberOfContacts));
            System.out.println(message);

            httpPost.setEntity(new StringEntity(message, "UTF8"));
            httpPost.setHeader("Content-type", "application/json");
            HttpResponse httpResponse = httpClient.execute(httpPost);
            System.out.println("Status line " + httpResponse.getStatusLine().getStatusCode());
        } catch (Exception e) {
            e.printStackTrace();

        }
    }

    public void reportMovement(Location location, int numberOfUsersInLocation) {
        HttpClient httpClient = new DefaultHttpClient();
        HttpPost httpPost = new HttpPost(this.reportMovementURI);

        try {
            String message = this.gson.toJson(new ReportMovementPostModel(this.districtName, location, numberOfUsersInLocation));
            System.out.println(message);

            httpPost.setEntity(new StringEntity(message, "UTF8"));
            httpPost.setHeader("Content-type", "application/json");
            HttpResponse httpResponse = httpClient.execute(httpPost);
            System.out.println("Status line " + httpResponse.getStatusLine().getStatusCode());
        } catch (Exception e) {
            e.printStackTrace();

        }
    }

    public void reportNewUser() {
        HttpClient httpClient = new DefaultHttpClient();
        HttpPost httpPost = new HttpPost(this.reportNewUserURI);

        try {
            String message = this.gson.toJson(new ReportNewUserPostModel(this.districtName));
            System.out.println(message);

            httpPost.setEntity(new StringEntity(message, "UTF8"));
            httpPost.setHeader("Content-type", "application/json");
            HttpResponse httpResponse = httpClient.execute(httpPost);
            System.out.println("Status line " + httpResponse.getStatusLine().getStatusCode());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
