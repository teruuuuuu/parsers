package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Tuple;

public class AndParser<X,Y> implements Parser<Tuple<X,Y>> {

    private final Parser<X> fparser;
    private final Parser<Y> sparser;

    public AndParser(Parser<X> fparser, Parser<Y> sparser) {
        this.fparser = fparser;
        this.sparser = sparser;
    }

    @Override
    public ParseResult<Tuple<X, Y>> parse(String input, int location) {
        switch (fparser.parse(input, location)) {
            case ParseResult.Success<X> fsuccess -> {
                switch (sparser.parse(input, fsuccess.next())) {
                    case ParseResult.Success<Y> ssuccess -> {
                        return new ParseResult.Success<>(new Tuple<>(fsuccess.value(), ssuccess.value()), ssuccess.next());
                    }
                    case ParseResult.Failure<Y> sfailure -> {
                        return new ParseResult.Failure<>(sfailure.message(), sfailure.next());
                    }
                }
            }
            case ParseResult.Failure<X> ffailure -> {
                return new ParseResult.Failure<>(ffailure.message(), ffailure.next());
            }
        }
    }
}
