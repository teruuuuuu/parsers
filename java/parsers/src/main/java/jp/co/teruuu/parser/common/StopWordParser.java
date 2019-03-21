package jp.co.teruuu.parser.common;

import java.util.stream.IntStream;

public class StopWordParser implements Parser<String> {
    private String literal;
    public StopWordParser(String literal) {
        this.literal = literal;
    }

    @Override
    public ParseResult<String> parse(String input) {
        Integer index = IntStream.range(0, input.length()).filter(i ->
                input.substring(i).startsWith(literal)
        ).findFirst().orElse(input.length());
        return new ParseResult.Success<>(input.substring(0, index), input.substring(index));
    }
}