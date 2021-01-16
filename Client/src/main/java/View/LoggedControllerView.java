package View;

import com.google.gson.Gson;

public class LoggedControllerView {
    public void init(String user){
        System.out.println(
                "##########################################################\n"
                        +  "##                         App                          ##\n"
                        +  "##########################################################\n"
                        +  "\n\n"
                        +  "Welcome " + user + "!\n"
        );
    }

    public void help(){
        System.out.println(
                "commands available:\n" +
                        "\tprobe (x) (y) - Number of people in the position (x,y) of the user district\n" +
                        "\tmove (x) (y) - Update the system of the new position (x,y) of the user\n" +
                        "\tsick - Notify the system that the user is infected with the virus\n" +
                        "\tsubscriptions - To show the current user's subscriptions\n" +
                        "\tsubscribe (subscription type) (params) - Subscribe to the subscription type\n" +
                        "\tunsubscribe (subscription type) (params) - Unsubscribe to the subscription type\n" +
                        "\t\tsubscription types:\n" +
                        "\t\t\tNobodyIn (district) (x) (y) - When there is no one at the position (x,y) of the (district)\n" +
                        "\t\t\tConcentrationIncIn (district) (x) (y) - When there is a rise in the number of people at the position (x,y) of the (district)\n" +
                        "\t\t\tConcentrationDecIn (district) (x) (y) - When there is a rise in the number of people at the position (x,y) of the (district)\n" +
                        "\t\t\tAnotherInfectedIn (district) - When there is another infected in the specified (district)\n" +
                        "\tlogoff - Log off from the current session into the main screen"
        );
    }

    public void showProbeView(int n, boolean success){
        if(success)
            System.out.println("The number of people in this location is " + n + "\n");
        else
            System.out.println("Error retrieving probe request");
    }

    public void showMoveView(boolean success){
        if(success)
            System.out.println("Moved successfully");
        else
            System.out.println("Error, unable to register the move");
    }

    public void showSickView(boolean success){
        if(success)
            System.out.println("Success, do not leave your home\nYou are going to be logged out automatically");
        else
            System.out.println("Error, pls try again or call the authorities, do not leave your home");
    }

    public void showLogOutView(){
        System.out.println("Logged out!");
    }

    public void showSubscribeView(boolean success){
        if(success)
            System.out.println("Successfully subscribed!");
        else
            System.out.println("Error, something went wrong, please try again");
    }

    public void showUnsubscribeView(boolean success){
        if(success)
            System.out.println("Successfully unsubscribed!");
        else
            System.out.println("Error, something went wrong, please try again");
    }

    public void showMaxSubscribedDistrictsWarning(){
        System.out.println("You have reached the maximum number of subscriptions to different districts (3)");
    }

    public void showContactWarning(){
        System.out.println("You have been in close contact with a person that had the virus");
    }

    public void showNotificationView(String json){
        Gson g = new Gson();
        NotificationObj obj = g.fromJson(json,NotificationObj.class);
        System.out.println("Notification arrived! --> " + obj.NotificationType + "//Its not here"); //TODO ERASE
        switch (obj.NotificationType) {
            case "InfectionsIncrease":
                showInfectionsIncreaseView(obj);
                break;
            case "ConcentrationIncreaseInLocation":
                showConcentrationIncreaseInLocationView(obj);
                break;
            case "ConcentrationDecreaseInLocation":
                showConcentrationDecreaseInLocationView(obj);
                break;
            case "NobodyInLocation":
                showNobodyInLocationView(obj);
                break;
        }


    }

    private void showNotificationBanner(){
        System.out.println(
                "##########################################################\n" +
                        "##                     Notification                     ##\n" +
                        "##########################################################\n"
        );
    }

    private void showInfectionsIncreaseView(NotificationObj obj){
        showNotificationBanner();
        System.out.println(
                "The number of infections in " + obj.district_name + " increased! Stay safe.\n"
        );
    }

    private void showConcentrationIncreaseInLocationView(NotificationObj obj){
        showNotificationBanner();
        System.out.println(
                "The number of people in " + obj.district_name + " at the position " +
                        "(" + obj.location.latitude + "," + obj.location.longitude + ")\n" +
                        "has increased! Stay safe.\n"
        );
    }

    private void showConcentrationDecreaseInLocationView(NotificationObj obj){
        showNotificationBanner();
        System.out.println(
                "The number of people in " + obj.district_name + " at the position " +
                        "(" + obj.location.latitude + "," + obj.location.longitude + ")\n" +
                        "has decreased! Stay safe.\n"
        );
    }

    private void showNobodyInLocationView(NotificationObj obj){
        showNotificationBanner();
        System.out.println(
                "The position " +
                        "(" + obj.location.latitude + "," + obj.location.longitude + ")" +
                        " in " + obj.district_name + "is empty! Stay safe.\n"
        );
    }

    private class NotificationObj{
        public posObj location;
        public String version;
        public String district_name;
        public String NotificationType;
    }

    private class posObj{
        public String latitude;
        public String longitude;
    }

}
