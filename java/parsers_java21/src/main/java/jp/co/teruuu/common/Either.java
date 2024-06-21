package jp.co.teruuu.common;

public sealed interface Either<X, Y> {
    record Left<X,Y> (X x) implements Either<X,Y> {}
    record Right<X,Y> (Y y) implements Either<X,Y> {}
}
