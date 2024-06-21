package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple3;

public class Pair3<X, Y, Z> implements Parser<Tuple3<X, Y, Z>> {
    private Parser<X> fhs;
    private Parser<Y> shs;
    private Parser<Z> ths;
    public Pair3(Parser<X> fhs, Parser<Y> shs, Parser<Z> ths) {
        this.fhs = fhs;
        this.shs = shs;
        this.ths = ths;
    }

    @Override
    public ParseResult<Tuple3<X, Y, Z>> parse(String input) {
        String next = input;
        ParseResult<X> fresult = fhs.parse(next);
        if(fresult instanceof ParseResult.Success<?>) {
            X fvalue = ((ParseResult.Success<X>) fresult).value;
            next = ((ParseResult.Success<X>) fresult).next;
            ParseResult<Y> sresult = shs.parse(next);
            if(sresult instanceof ParseResult.Success<?>) {
                Y svalue = ((ParseResult.Success<Y>) sresult).value;
                next = ((ParseResult.Success<Y>) sresult).next;
                ParseResult<Z> tresult = ths.parse(next);
                if(tresult instanceof ParseResult.Success<?>) {
                    Z tvalue = ((ParseResult.Success<Z>) tresult).value;
                    next = ((ParseResult.Success<Z>) tresult).next;
                    return new ParseResult.Success<>(new Tuple3(fvalue, svalue, tvalue), next);
                } else {
                    return (ParseResult.Failure) tresult;
                }
            } else {
                return (ParseResult.Failure) sresult;
            }
        } else {
            return (ParseResult.Failure) fresult;
        }
    }
}