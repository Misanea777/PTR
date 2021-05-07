package main.java;

import main.java.protocol.Message;
import main.java.protocol.MsgType;
import main.java.protocol.options.PersistentMsg;
import org.bson.Document;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public class TCPClient {

    private Socket clientSocket;
    private DataOutputStream output;
    private Scanner sc;


    public TCPClient(String ip, int port) throws IOException {
        clientSocket = new Socket(ip, port);

        output = new DataOutputStream(clientSocket.getOutputStream());
        sc = new Scanner(System.in);
    }

    public void start() throws IOException {
        while(true) {
            byte[] data = sc.nextLine().getBytes(StandardCharsets.UTF_8);
            send(data);
        }
    }

    public void send(byte[] data) throws IOException {
        output.writeInt(data.length);
        output.write(data,0,data.length);
    }

    public void send(Document document) {
        String[] topics = {"tweets"};
        Message message = new Message.Builder(MsgType.DATA, topics)
                .withOption(new PersistentMsg(true))
                .withBody(document)
                .build();


        try {
            send(message.toByte());
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


}
