package main.java.protocol.options;

import org.json.simple.JSONObject;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ExpirationDate implements Option<LocalDateTime> {

    private static final String OPT_NAME = "expiration_date";
    private LocalDateTime value = LocalDateTime.now();

    public ExpirationDate() {
        this.value = value.plusMinutes(10);
    }

    public ExpirationDate(LocalDateTime value) {
        this.value = value;
    }

    @Override
    public String getOptionName() {
        return OPT_NAME;
    }

    @Override
    public LocalDateTime getOptionValue() {
        return value;
    }

    @Override
    public void setOptionName(String name) { // not allowed

    }

    @Override
    public void setOptionValue(LocalDateTime value) {
        this.value = value;
    }

    @Override
    public JSONObject toJson() {
        JSONObject object = new JSONObject();
        int year = value.getYear();
        int month = value.getMonthValue();
        int date =  value.getDayOfMonth();
        int hour = value.getHour();
        int minute = value.getMinute();
        int second = value.getSecond();
        List<Integer> time = Arrays.asList(year, month, date, hour, minute, second);
        object.put(OPT_NAME, time);
        return object;
    }
}
