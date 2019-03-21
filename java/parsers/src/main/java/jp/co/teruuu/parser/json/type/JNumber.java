package jp.co.teruuu.parser.json.type;

public class JNumber implements JValue{
    public int value = 0;
    public int decimal = 0;
    public int base = 0;

    public JNumber(int value, int decimal, int base) {
        this.value = value;
        this.decimal = decimal;
        this.base = base;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof JNumber && this.value == ((JNumber) other).value &&
                this.decimal == ((JNumber) other).decimal &&
                this.base == ((JNumber) other).base;
    }
}
