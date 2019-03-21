package jp.co.teruuu.parser.common.type;

public class Tuple3 <X, Y, Z> {
    public final X item1;
    public final Y item2;
    public final Z item3;

    public Tuple3(X item1, Y item2, Z item3) {
        this.item1 = item1;
        this.item2 = item2;
        this.item3 = item3;
    }

    @Override
    public int hashCode() {
        int result = item1.hashCode();
        result = 31 * result + item2.hashCode();
        result = 31 * result + item3.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Tuple3{item1=" + item1 + ", item2=" + item2 + ", item3=" + item3 + "}";
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof Tuple3 && this.item1.equals(((Tuple3)other).item1) &&
                this.item2.equals(((Tuple3)other).item2) &&
                this.item3.equals(((Tuple3)other).item3);
    }
}