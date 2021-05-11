package main.java.protocol.options;

import org.json.simple.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class Topics implements Option<List<String>>{

    private static final String OPT_NAME = "topics";
    private List<String> value = new ArrayList<String>();

    public Topics() {
    }

    public Topics(List<String> value) {
        this.value = value;
    }

    @Override
    public String getOptionName() {
        return OPT_NAME;
    }

    @Override
    public List<String> getOptionValue() {
        return value;
    }

    @Override
    public void setOptionName(String name) {

    }

    @Override
    public void setOptionValue(List<String> value) {
        this.value = value;
    }

    @Override
    public JSONObject toJson() {
        JSONObject object = new JSONObject();
        if (value.size() > 0) object.put(OPT_NAME, value);
        return object;
    }
}
