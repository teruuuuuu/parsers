package jp.co.teruuu.parser.common;

import java.util.ArrayList;
import java.util.List;

/**
 * 1個以上の連続
 * @param <T>
 */
public class PlusParser<T> implements Parser<List<T>> {
    private Parser<T> parser;
    public PlusParser(Parser<T> parser) {
        this.parser = parser;
    }

    @Override
    public ParseResult<List<T>> parse(String input) {
        ParseResult<T> result;
        String next = input;
        List<T> values = new ArrayList<T>();
        while(true) {
            String previous = next;
            result = parser.parse(next);
            if(result instanceof ParseResult.Success<?>) {
                ParseResult.Success<T> success = (ParseResult.Success<T>)result;
                values.add(success.value);
                next = success.next;
            } else {
                next = null;
            }
            if(next == null) {
                if(values.size() == 0) {
                    return (ParseResult.Failure) result;
                } else {
                    return new ParseResult.Success<>(values, previous);
                }
            }
        }
    }
}