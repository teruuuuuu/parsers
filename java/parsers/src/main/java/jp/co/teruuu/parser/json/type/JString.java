package jp.co.teruuu.parser.json.type;

public class JString implements JValue{
    public String value;
    public JString(String value){
        this.value = value;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof JString && this.value.equals(((JString) other).value);
    }
}
