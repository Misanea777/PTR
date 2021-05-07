package main.java.protocol.options;

import org.json.simple.JSONObject;


//if adding this opt and seting value to false, its the same as if not adding at all
public class PersistentMsg implements Option<Boolean> {

    private static final String OPT_NAME = "presistent";
    private Boolean value = false;

    public PersistentMsg() {
    }

    public PersistentMsg(Boolean value) {
        this.value = value;
    }

    @Override
    public String getOptionName() {
        return OPT_NAME;
    }

    @Override
    public Boolean getOptionValue() {
        return value;
    }

    @Override
    public void setOptionName(String name) {  // do nothing, this name cannot be changed

    }

    @Override
    public void setOptionValue(Boolean value) {
        this.value = value;
    }

    @Override
    public JSONObject toJson() {
        JSONObject object = new JSONObject();
        if (value) object.put(OPT_NAME, value);
        return object;
    }
}
