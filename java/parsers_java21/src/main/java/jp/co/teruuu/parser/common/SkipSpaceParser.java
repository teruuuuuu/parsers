package jp.co.teruuu.parser.common;

public class SkipSpaceParser implements Parser<Void> {
    @Override
    public ParseResult<Void> parse(String input, int location) {
        char c;
        while (true) {
            if (input.length() == location) {
                break;
            }

            c = input.charAt(location);
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                location++;
            } else {
                break;
            }
        }
        return new ParseResult.Success<>(null, location);
    }
}
