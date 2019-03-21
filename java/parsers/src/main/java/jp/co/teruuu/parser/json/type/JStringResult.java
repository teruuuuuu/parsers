package jp.co.teruuu.parser.json.type;

public class JStringResult {
    public String value;
    public JStringResult(String value){
        this.value = value;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof JStringResult && this.value.equals(((JStringResult) other).value);
    }
}
