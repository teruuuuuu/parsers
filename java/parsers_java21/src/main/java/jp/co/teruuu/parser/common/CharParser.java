package jp.co.teruuu.parser.common;

public class CharParser implements Parser<String> {

    final char c;

    public CharParser(char c) {
        this.c = c;
    }
    @Override
    public ParseResult<String> parse(String input, int location) {
        if (input.length() > location && input.charAt(location) == c) {
            return new ParseResult.Success<>(String.valueOf(c), location + 1);
        } else {
            return new ParseResult.Failure<>(String.format("not (char=[%s], loc=[%d]), input=%s", c, location, input), location);
        }
    }
}
