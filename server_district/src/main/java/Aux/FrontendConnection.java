package Aux;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class FrontendConnection {
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;

    public FrontendConnection(String ip, int port) throws IOException {
        this.socket = new Socket(ip,port);
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(this.socket.getOutputStream());
    }

    public String read() throws IOException {
        return this.in.readLine();
    }

    public void write(String line){
        this.out.println();
    }
}
