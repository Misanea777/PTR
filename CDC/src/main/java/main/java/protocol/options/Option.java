package main.java.protocol.options;

import org.json.simple.JSONObject;

public interface Option<T> {
    String getOptionName();
    T getOptionValue();
    void setOptionName(String name);
    void setOptionValue(T value);

    JSONObject toJson();

}


