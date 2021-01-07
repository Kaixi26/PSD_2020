import java.io.*;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;

public class AnnounceDistrictServerTest {
    public static void main(String[] args) throws IOException {
        InetAddress addr = InetAddress.getByName("127.0.0.1");
        ServerSocket serverSocket = new ServerSocket(12345, 5, addr);
        while(true) {
            Socket socket = serverSocket.accept();

            PrintWriter p = new PrintWriter(socket.getOutputStream());

            new Thread(new Reader(socket)).start();
            Scanner sc = new Scanner(System.in);
            while(true) {
                p.println(sc.nextLine());
                p.flush();
            }

            /*
            p.println("{\"username\":\"lazaro\",\"location\":{\"latitude\":1,\"longitude\":1},\"RequestType\":\"NotifyLocation\",\"version\":\"1.0.0\"}");
            p.flush();

            p.println("{\"ResponseType\":\"AnnounceDistrictServer\",\"code\":200,\"version\":\"1.0.0\"}");
            p.flush();*/
        }
    }
}
