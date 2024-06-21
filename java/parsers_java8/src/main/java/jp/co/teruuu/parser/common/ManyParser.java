package jp.co.teruuu.parser.common;

import java.util.ArrayList;
import java.util.List;

/**
 * 0個以上の連続をパースする
 *
 * @param <T>
 */
public class ManyParser<T> implements Parser<List<T>> {
    private Parser<T> parser;

    public ManyParser(Parser<T> parser) {
        this.parser = parser;
    }

    @Override
    public ParseResult<List<T>> parse(String input) {
        ParseResult<T> result;
        String next = input;
        List<T> values = new ArrayList<T>();
        while (true) {
            String previous = next;
            result = parser.parse(next);
            if (result instanceof ParseResult.Success<?>) {
                ParseResult.Success<T> success = (ParseResult.Success<T>) result;
                values.add(success.value);
                next = success.next;
            } else {
                next = null;
            }
            if (next == null) {
                return new ParseResult.Success<>(values, previous);
            }
        }
    }
}