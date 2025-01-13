package jp.co.teruuu.parser.common;

import java.util.List;
import java.util.stream.Stream;

public class ArrayParser<T, X, Y, Z> implements Parser<List<T>> {

    private final Parser<List<T>> parser;
    public ArrayParser(Parser<T> parser, Parser<X> lbrackets, Parser<Y> rbrackets, Parser<Z> separtor) {
        this.parser = lbrackets.andRight(
                parser.and(separtor.andRight(parser).seq()).optional()
                        .map(value -> value.map(v -> Stream.concat(Stream.of(v._1()), v._2().stream()).toList()).orElse(List.of()))
        ).andLeft(rbrackets);
    }

    @Override
    public ParseResult<List<T>> parse(String input, int location) {
        return this.parser.parse(input, location);
    }
}
