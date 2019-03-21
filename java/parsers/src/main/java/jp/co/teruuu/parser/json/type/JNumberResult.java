package jp.co.teruuu.parser.json.type;

import jp.co.teruuu.parser.json.JNumber;

public class JNumberResult {
    public int value;
    public int decimal;
    public int base;

    public JNumberResult(int value, int decimal, int base) {
        this.value = value;
        this.decimal = decimal;
        this.base = base;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof JNumberResult && this.value == ((JNumberResult) other).value &&
                this.decimal == ((JNumberResult) other).decimal &&
                this.base == ((JNumberResult) other).base;
    }
}
