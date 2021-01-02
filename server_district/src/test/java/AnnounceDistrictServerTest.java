import java.io.*;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class AnnounceDistrictServerTest {
    public static void main(String[] args) throws IOException {
        InetAddress addr = InetAddress.getByName("127.0.0.1");
        ServerSocket serverSocket = new ServerSocket(12345, 5, addr);
        while(true) {
            Socket socket = serverSocket.accept();
            BufferedReader bf = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter p = new PrintWriter(socket.getOutputStream());
            String l;
            while ((l = bf.readLine()) != null) {
                System.out.println(l);
                p.println("{\"ResponseType\":\"AnnounceDistrictServer\",\"code\":200,\"version\":\"1.0.0\"}");
                p.flush();
            }
        }
    }
}
