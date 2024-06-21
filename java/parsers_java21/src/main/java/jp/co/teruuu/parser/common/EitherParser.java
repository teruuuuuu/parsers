package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Either;

import java.util.List;

public class EitherParser<X,Y> implements Parser<Either<X, Y>> {

    private final Parser<X> fparser;
    private final Parser<Y> sparser;

    public EitherParser(Parser<X> fparser, Parser<Y> sparser) {
        this.fparser = fparser;
        this.sparser = sparser;
    }
    @Override
    public ParseResult<Either<X, Y>> parse(String input, int location) {
        switch (fparser.parse(input, location)) {
            case ParseResult.Success<X> fsuccess -> {
                return new ParseResult.Success<>(new Either.Left<>(fsuccess.value()), fsuccess.next());
            }
            case ParseResult.Failure<X> ffailure -> {
                switch (sparser.parse(input, location)) {
                    case ParseResult.Success<Y> ssuccess -> {
                        return new ParseResult.Success<>(new Either.Right<>(ssuccess.value()), ssuccess.next());
                    }
                    case ParseResult.Failure<Y> sfailure -> {
                        return new ParseResult.Failure<>(String.join(",", List.of(ffailure.message(), sfailure.message())), location);
                    }
                }
            }
        }
    }
}
