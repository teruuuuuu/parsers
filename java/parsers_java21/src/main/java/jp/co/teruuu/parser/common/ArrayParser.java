package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Tuple;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class ArrayParser<T, X, Y, Z> implements Parser<List<T>> {

    private final Parser<List<T>> parser;
    private final Parser<X> lbrackets;
    private final Parser<Y> rbrackets;
    private final Parser<Z> separtor;

    public ArrayParser(Parser<T> parser, Parser<X> lbrackets, Parser<Y> rbrackets, Parser<Z> separtor) {
        this.lbrackets = lbrackets.withSkipSpace();
        this.rbrackets = rbrackets.withSkipSpace();
        this.separtor = separtor.withSkipSpace();

        Parser<T> parser1 = parser.withSkipSpace();
        Parser<T> parser2 = (input, location) -> {
            switch (this.separtor.and(parser1).parse(input, location)) {
                case ParseResult.Success<Tuple<Z, T>> success -> {
                    return new ParseResult.Success<>(success.value().snd(), success.next());
                }
                case ParseResult.Failure<Tuple<Z, T>> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
        Parser<Optional<Tuple<T, List<T>>>> parser3 = parser1.and(parser2.seq()).optional();
        this.parser = (input, location) -> {
            switch (parser3.parse(input, location)) {
                case ParseResult.Success<Optional<Tuple<T, List<T>>>> success -> {
                    return new ParseResult.Success<>(
                            success.value().map(v ->
                                    Stream.concat(Stream.of(v.fst()), v.snd().stream()).toList()).orElse(List.of()),
                            success.next());
                }
                case ParseResult.Failure<Optional<Tuple<T, List<T>>>> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
    }

    @Override
    public ParseResult<List<T>> parse(String input, int location) {
        ParseResult<X> xParseResult = lbrackets.parse(input, location);
        if (xParseResult instanceof ParseResult.Failure<X> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        ParseResult.Success<X> xSuccess = (ParseResult.Success<X>) xParseResult;
        location = xSuccess.next();

        ParseResult<List<T>> listParseResult = parser.parse(input, location);
        if (listParseResult instanceof ParseResult.Failure<List<T>> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        ParseResult.Success<List<T>> listSuccess = (ParseResult.Success<List<T>>) listParseResult;
        location = listSuccess.next();


        ParseResult<Y> yParseResult = rbrackets.parse(input, location);
        if (yParseResult instanceof ParseResult.Failure<Y> failure) {
            return new ParseResult.Failure<>(failure.message(), location);
        }
        ParseResult.Success<Y> ySuccess = (ParseResult.Success<Y>) yParseResult;
        location = ySuccess.next();

        return new ParseResult.Success<>(listSuccess.value(), location);
    }
}
