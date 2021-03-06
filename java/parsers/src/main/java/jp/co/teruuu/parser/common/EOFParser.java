package jp.co.teruuu.parser.common;

public class EOFParser implements Parser<String> {
    @Override
    public ParseResult<String> parse(String input) {
        if(input.length() != 0) {
            return new ParseResult.Failure<>("expected: EOF, actual: " + input.charAt(0), input);
        } else {
            return new ParseResult.Success<String>("", "");
        }
    }
}