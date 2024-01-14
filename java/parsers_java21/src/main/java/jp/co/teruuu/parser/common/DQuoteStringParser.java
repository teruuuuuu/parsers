package jp.co.teruuu.parser.common;

public class DQuoteStringParser implements Parser<String> {
    private final Parser<String> parser;
    public DQuoteStringParser() {
        this.parser = Parser.charP('"').andRight(
                Parser.escape().or(Parser.charP('"').not()).seq().map(value -> String.join("", value))
        ).andLeft(Parser.charP('"'));
    }

    @Override
    public ParseResult<String> parse(String input, int location) {
        return parser.parse(input, location);
    }
}
