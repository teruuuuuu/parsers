package jp.co.teruuu.parser.common;

public class EscapeParser implements Parser<String> {
    @Override
    public ParseResult<String> parse(String input, int location) {
        if (input.length() >= location + 1 && input.startsWith("\\", location)) {
            char c = input.charAt(location + 1);
            if (c == ' ') {
                return new ParseResult.Success<>(String.valueOf(' '), location + 2);
            } else if (c == 't') {
                return new ParseResult.Success<>(String.valueOf('\t'), location + 2);
            } else if (c == 'f') {
                return new ParseResult.Success<>(String.valueOf('\f'), location + 2);
            } else if (c == 'b') {
                return new ParseResult.Success<>(String.valueOf('\b'), location + 2);
            } else if (c == 'r') {
                return new ParseResult.Success<>(String.valueOf('\r'), location + 2);
            } else if (c == 'n') {
                return new ParseResult.Success<>(String.valueOf('\n'), location + 2);
            } else if (c == '\\') {
                return new ParseResult.Success<>(String.valueOf('\\'), location + 2);
            } else if (c == '"') {
                return new ParseResult.Success<>(String.valueOf('"'), location + 2);
            } else if (c == '\'') {
                return new ParseResult.Success<>(String.valueOf('\''), location + 2);
            }
        }
        return new ParseResult.Failure<>(String.format("not escape location=[%d] input=[%s]", location, input), location);
    }
}
