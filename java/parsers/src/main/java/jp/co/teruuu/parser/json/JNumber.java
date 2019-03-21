package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Or;
import jp.co.teruuu.parser.common.Pair2;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.json.type.JNumberResult;

import java.util.List;
import java.util.Optional;

public class JNumber implements JParser<JNumberResult> {
    Parser<Optional<String>> sign;
    Parser<Tuple2<Tuple2<String, List<String>>, Optional<Tuple2<String, List<String>>>>> floats;
    Parser<Optional<Tuple2<Tuple2<String, Optional<String>>, List<String>>>> base;


    public JNumber() {
        Parser<String> oton = Parser.string("1").or(Parser.string("2")).or(Parser.string("3")).
                or(Parser.string("4")).or(Parser.string("5")).or(Parser.string("6")).
                or(Parser.string("7")).or(Parser.string("8")).or(Parser.string("9"));
        Parser<String> zton = oton.or(Parser.string("0"));

        this.sign = (new Or(Parser.string("+"), Parser.string("-"))).option();
        this.floats = oton.pair2(zton.many()).pair2(Parser.string(".").pair2(zton.plus()).option());
        this.base = new Pair2(Parser.string("E").or(Parser.string("e")).pair2(sign), zton.plus()).option();
    }

    @Override
    public ParseResult<JNumberResult> parse(String input) {
        String next = input;
        ParseResult<Optional<String>> signParse = this.sign.parse(next);
        if(signParse instanceof ParseResult.Success) {
            Optional<String> signValue = ((ParseResult.Success<Optional<String>>) signParse).value;
            next = ((ParseResult.Success) signParse).next;

            ParseResult<Tuple2<Tuple2<String, List<String>>, Optional<Tuple2<String, List<String>>>>> floatParse = this.floats.parse(next);
            if(floatParse instanceof ParseResult.Success) {
                Tuple2<Tuple2<String, List<String>>, Optional<Tuple2<String, List<String>>>> floatValue = ((ParseResult.Success<Tuple2<Tuple2<String, List<String>>, Optional<Tuple2<String, List<String>>>>>) floatParse).value;
                next = ((ParseResult.Success<Tuple2<Tuple2<String, List<String>>, Optional<Tuple2<String, List<String>>>>>) floatParse).next;

                ParseResult baseParse = this.base.parse(next);
                if(baseParse instanceof ParseResult.Success) {
                    Optional<Tuple2<Tuple2<String, Optional<String>>, List<String>>> baseValue = ((ParseResult.Success<Optional<Tuple2<Tuple2<String, Optional<String>>, List<String>>>>) baseParse).value;
                    next = ((ParseResult.Success) baseParse).next;

                    String valueStr = "";
                    int value = 0;
                    String deciStr = "";
                    int decimal = 0;
                    int base = 0;

                    if(signValue.isPresent() && signValue.get().equals("-")) {
                        valueStr = "-";
                    }
                    valueStr += floatValue.item1.item1;
                    for(String s: floatValue.item1.item2) {
                        valueStr += s;
                    }
                    value = Integer.valueOf(valueStr);

                    if(floatValue.item2.isPresent()) {
                        for(String s: floatValue.item2.get().item2) {
                            deciStr += s;
                        }
                        decimal = Integer.valueOf(deciStr);
                    }

                    if(baseValue.isPresent()) {
                        String baseStr = "";
                        if(baseValue.get().item1.item2.isPresent() && baseValue.get().item1.item2.get().equals("-")) {
                            baseStr += "-";
                        }
                        for(String s: baseValue.get().item2) {
                            baseStr += s;
                        }
                        base = Integer.valueOf(baseStr);
                    }
                    return new ParseResult.Success<>(new JNumberResult(value, decimal, base), next);
                } else {
                    return (ParseResult.Failure) baseParse;
                }
            } else {
                return (ParseResult.Failure) floatParse;
            }
        } else {
            return (ParseResult.Failure) signParse;
        }
    }
}
