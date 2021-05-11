package main.java;

import main.java.protocol.Message;
import main.java.protocol.MsgType;
import main.java.protocol.options.ExpirationDate;
import main.java.protocol.options.PersistentMsg;
import main.java.protocol.options.Topics;
import org.bson.Document;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class TCPClient {

    private Socket clientSocket;
    private DataOutputStream output;
    private Scanner sc;


    public TCPClient(String ip, int port) throws IOException {
        clientSocket = new Socket(ip, port);

        output = new DataOutputStream(clientSocket.getOutputStream());
        start();

        sc = new Scanner(System.in);
    }

    public void start() {
        List<String> topics = Arrays.asList("tweets", "users");
        Message message = new Message.Builder(MsgType.conn_pub)
                .withOption(new Topics(topics))
                .withOption(new ExpirationDate(LocalDateTime.now().plusMinutes(10)))
                .build();

        try {
            send(message.toByte());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    public void end() {
        String[] topics = {};
        Message message = new Message.Builder(MsgType.disconn_pub)
                .build();

        try {
            send(message.toByte());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void send(byte[] data) throws IOException {
        output.writeInt(data.length);
        output.write(data,0,data.length);
    }

    public void send(Document document) {
        List<String> topics = Arrays.asList("tweets");
        Message message = new Message.Builder(MsgType.data)
                .withOption(new Topics(topics))
                .withOption(new PersistentMsg(true))
                .withOption(new ExpirationDate())
                .withBody(document)
                .build();


        try {
            send(message.toByte());
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


}
