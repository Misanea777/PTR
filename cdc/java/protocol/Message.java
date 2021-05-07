package main.java.protocol;

import main.java.protocol.options.Option;
import org.bson.Document;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.nio.charset.StandardCharsets;
import java.util.*;

public class Message {

    private MsgType msgType;  //req
    private String[] topics;  //req
    private List<Option> opt;     //opt
    private JSONObject body;      //it depends

    private Message(Builder builder) {
        this.msgType = builder.msgType;
        this.topics = builder.topics;
        this.opt = builder.opt;
        this.body = builder.body;
    }

    public MsgType getMsgType() {
        return msgType;
    }

    public String[] getTopics() {
        return topics;
    }

    public List<Option> getOpt() {
        return opt;
    }

    public JSONObject getBody() {
        return body;
    }

    public JSONArray toJson() {
        JSONObject header = new JSONObject();
        header.put("type", msgType);
        header.put("topics", Arrays.asList(topics));
        if (opt.size() > 0) header.put("opt", toJsonList(opt));

        JSONObject msgBody = new JSONObject();
        if(msgType.isInTypeGroup(MsgType.TypeGroup.DATA)) {
            msgBody.putAll(body);
        }

        JSONArray message = new JSONArray();
        message.add(header);
        message.add(msgBody);

        return message;
    }

    @Override
    public String toString() {
        return toJson().toJSONString();
    }

    public byte[] toByte() {
        return toString().getBytes(StandardCharsets.UTF_8);
    }

    public List<JSONObject> toJsonList(List<Option> list) {
        List<JSONObject> jsonList = new ArrayList<JSONObject>();
        list.forEach(it -> {if (!it.toJson().isEmpty()) jsonList.add(it.toJson());});
        return jsonList;
    }

    public static class Builder {
        private final MsgType msgType;  //req
        private final String[] topics;  //req
        private List<Option> opt = new ArrayList<Option>();     //opt
        private JSONObject body;      //it depends

        public Builder(MsgType msgType, String[] topics) {
            this.msgType = msgType;
            this.topics = topics;
        }

        public Builder withOptions(List<Option> options) {
            this.opt.addAll(options);
            return this;
        }

        public Builder withOption(Option option) {
            this.opt.add(option);
            return this;
        }


        public Builder withBody(Document body) {
            if (msgType.isInTypeGroup(MsgType.TypeGroup.CONTROL)) return this;

            try {
                this.body = (JSONObject) new JSONParser().parse(body.toJson());
                body.remove("_id");
            } catch (ParseException e) {
                e.printStackTrace();
            }
            return this;
        }

        public Message build() {
            Message user =  new Message(this);
            return user;
        }
    }
}







