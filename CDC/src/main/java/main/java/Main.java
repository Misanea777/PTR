package main.java;

import java.io.IOException;

public class Main {

    public static void main(String args[]) throws IOException {
        TCPClient client = new TCPClient("localhost", 4020);

        DBConnection conn = new DBConnection(client);

    }
}
