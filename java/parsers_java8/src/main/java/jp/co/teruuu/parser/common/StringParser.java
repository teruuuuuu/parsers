package jp.co.teruuu.parser.common;

public class StringParser implements Parser<String> {
    public final String literal;
    public StringParser(String literal) {
        this.literal = literal;
    }

    @Override
    public ParseResult<String> parse(String input) {
        if(input.startsWith(literal)) {
            return new ParseResult.Success<>(literal, input.substring(literal.length()));
        }else {
            return new ParseResult.Failure<>("expect: " + literal, input);
        }
    }

}