package main.java;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import com.mongodb.client.ChangeStreamIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.changestream.ChangeStreamDocument;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

import static com.mongodb.client.model.Aggregates.match;
import static java.util.Collections.singletonList;

public class DBConnection {

    MongoClient client;
    MongoDatabase db;
    ChangeStreamIterable<Document> changeStream;
    TCPClient tcpClient;

    public DBConnection(TCPClient tcpClient) {
        this.tcpClient = tcpClient;
        MongoClientURI uri = new MongoClientURI(
                "mongodb+srv://erl_user:erl_user@cluster0.ux9po.mongodb.net/myFirstDatabase?retryWrites=true&w=majority");
        client = new MongoClient(uri);
        db = client.getDatabase("Erlang");

        List<Bson> pipeline = singletonList(match(Filters.eq("operationType", "insert")));
        MongoCollection<Document> tweets = db.getCollection("tweets");
        changeStream = tweets.watch(pipeline);
        changeStream.forEach(consumer);

    }

    Consumer<ChangeStreamDocument<Document>> consumer = new Consumer<ChangeStreamDocument<Document>>() {
        @Override
        public void accept(ChangeStreamDocument<Document> documentChangeStreamDocument) {
            Document document = documentChangeStreamDocument.getFullDocument();
            tcpClient.send(document);
        }
    };

}
