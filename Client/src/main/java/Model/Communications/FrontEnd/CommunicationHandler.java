package Model.Communications.FrontEnd;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;

import java.io.*;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;

public class CommunicationHandler {

    private Socket socket;
    private BlockingQueue<String> queue;
    private BlockingQueue<String> notificationsQueue;

    public CommunicationHandler(Socket socket){
        this.socket=socket;
        this.queue = new LinkedBlockingDeque<>();
        this.notificationsQueue = new LinkedBlockingDeque<>();
        Thread receiver = new Thread(new Receiver(queue));
        receiver.start();
    }

    public void clearNotificationQueue(){
        this.notificationsQueue.clear();
    }


    public void sendRequest(String json){
        try{
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            out.println(json);
            //System.out.println("SENT --> " + json + "//Its not here"); //TODO ERASE
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public String receiveResponse(){
        try {
            return queue.take();
        } catch (InterruptedException e) {
            return "error";
        }
    }

    public String receiveNotification(){
        try {
            return notificationsQueue.take();
        } catch (InterruptedException e) {
            return "error";
        }
    }

    public class Receiver implements Runnable {
        private BlockingQueue<String> queue;

        public Receiver(BlockingQueue<String> queue){
            this.queue = queue;
        }

        @Override
        public void run() {

            try {
                BufferedReader in = new BufferedReader(
                        new InputStreamReader(socket.getInputStream()));
                while (true){
                    String json = in.readLine();
                    //System.out.println("Arrived --> " + json + "//Its not here"); //TODO ERASE
                    if(json.contains("\"ReplyType\": \"Notification\"")) {
                        notificationsQueue.put(json);
                    }
                    else
                        queue.put(json);
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
}
