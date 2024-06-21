package jp.co.teruuu.parser.common;

public class IntegerParser implements Parser<Integer> {

    private final int ZERO_CODE = '0';
    private final int NINE_CODE = '9';

    @Override
    public ParseResult<Integer> parse(String input, int location) {
        boolean isNumber = false;
        int num = 0;
        while (input.charAt(location) >= ZERO_CODE && input.charAt(location) <= NINE_CODE) {
            num *= 10;
            num += (input.charAt(location) - ZERO_CODE);
            location++;
            isNumber = true;
        }
        if (isNumber) {
            return new ParseResult.Success<>(num, location);
        } else {
            return new ParseResult.Failure<>(String.format("not number location=[%d] input=[%s]", location, input), location);
        }
    }
}
