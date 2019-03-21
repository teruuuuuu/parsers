package jp.co.teruuu.parser.common;

public class Or<X> implements Parser<X> {
    private Parser<X> lhs;
    private Parser<X> rhs;
    public Or(Parser<X> lhs, Parser<X> rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public ParseResult<X> parse(String input) {
        ParseResult<X> lresult = lhs.parse(input);
        if(lresult instanceof ParseResult.Failure<?>) {
            return rhs.parse(input);
        } else {
            return lresult;
        }
    }
}