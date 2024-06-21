package jp.co.teruuu.parser.common;

import java.util.List;

public class OrParser<X> implements Parser<X> {
    private final Parser<X> fparser;
    private final Parser<X> sparser;

    public OrParser(Parser<X> fparser, Parser<X> sparser) {
        this.fparser = fparser;
        this.sparser = sparser;
    }

    @Override
    public ParseResult<X> parse(String input, int location) {
        switch (fparser.parse(input, location)) {
            case ParseResult.Success<X> fsuccess -> {
                return fsuccess;
            }
            case ParseResult.Failure<X> ffailure -> {
                switch (sparser.parse(input, location)) {
                    case ParseResult.Success<X> ssuccess -> {
                        return ssuccess;
                    }
                    case ParseResult.Failure<X> sfailure -> {
                        return new ParseResult.Failure<>(String.join(",", List.of(ffailure.message(), sfailure.message())), location);
                    }
                }
            }
        }
    }
}
