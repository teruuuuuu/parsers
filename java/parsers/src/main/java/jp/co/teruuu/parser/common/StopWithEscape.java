package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple2;

import java.util.List;

public class StopWithEscape implements Parser<String> {
    private String stop;
    private List<Tuple2<String, String>> escapes;
    public StopWithEscape(String stop, List<Tuple2<String, String>> escapes) {
        this.stop = stop;
        this.escapes = escapes;
    }

    @Override
    public ParseResult<String> parse(String input) {
        String value = "";
        String next = input;
        while(true) {
            for( Tuple2<String, String> escape : this.escapes) {
                if(next.indexOf(escape.item1) == 0) {
                    value += escape.item2;
                    next = next.substring(escape.item1.length());
                }
            }
            if(next.indexOf(this.stop) != 0 && next.length() > 0){
                value += next.substring(0, 1);
                next = next.substring(1);
            } else {
                return new ParseResult.Success<>(value, next);
            }
        }
    }
}