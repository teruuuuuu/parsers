package jp.co.teruuu.parser.common.type;

public class Tuple2<X, Y> {
    public final X item1;
    public final Y item2;

    public Tuple2(X item1, Y item2) {
        this.item1 = item1;
        this.item2 = item2;
    }

    @Override
    public int hashCode() {
        int result = item1.hashCode();
        result = 31 * result + item2.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Tuple2{item1=" + item1 + ", item2=" + item2 + "}";
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof Tuple2 && this.item1.equals(((Tuple2)other).item1) && this.item2.equals(((Tuple2)other).item2);
    }
}