package Model.Communications.FrontEnd;

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
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public String receiveResponse(){
        try {
            return queue.take();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return "error";
    }

    public String receiveNotification(){
        try {
            return notificationsQueue.take();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return "error";
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
                    if(json.contains("Notification")) //TODO change this to be more specific
                        notificationsQueue.put(json);
                    else
                        queue.put(json);
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }


    }
}
