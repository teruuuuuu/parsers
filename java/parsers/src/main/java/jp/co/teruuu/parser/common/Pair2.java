package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple2;

public class Pair2<X, Y> implements Parser<Tuple2<X, Y>> {
    private Parser<X> lhs;
    private Parser<Y> rhs;
    public Pair2(Parser<X> lhs, Parser<Y> rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public ParseResult<Tuple2<X, Y>> parse(String input) {
        ParseResult<X> lresult = lhs.parse(input);

        if(lresult instanceof ParseResult.Success<?>) {
            X value1 = ((ParseResult.Success<X>)lresult).value;
            String next1 = ((ParseResult.Success<X>)lresult).next;
            ParseResult<Y> rresult = rhs.parse(next1);

            if(rresult instanceof ParseResult.Success<?>) {
                Y value2 = ((ParseResult.Success<Y>)rresult).value;
                String next2 = ((ParseResult.Success<Y>) rresult).next;
                return new ParseResult.Success(new Tuple2(value1, value2), next2);
            } else {
                return (ParseResult.Failure) rresult;
            }
        } else {
            return (ParseResult.Failure)lresult;
        }
    }
}