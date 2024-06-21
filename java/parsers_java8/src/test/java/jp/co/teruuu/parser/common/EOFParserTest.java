package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.common.type.Tuple2;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

public class EOFParserTest {

    @Test
    public void test(){
        Parser<Tuple2<List<String>, String>> parser = Parser.string("Hello").many().pair2(Parser.EOF());

        ParseResult result1 = parser.parse("HelloHelloHello");
        assert(result1 instanceof ParseResult.Success);
        assert(((ParseResult.Success<Tuple2<List<String>, String>>) result1).value.item1.equals(Arrays.asList("Hello", "Hello","Hello")));
        assert(((ParseResult.Success<Tuple2<List<String>, String>>) result1).value.item2.equals(""));

        ParseResult result2 = parser.parse("Hell oHelloHello");
        assert(result2 instanceof ParseResult.Failure);


    }
}
