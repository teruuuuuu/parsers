package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;

import java.util.List;
import java.util.Optional;

public class Main {
    public static void main(String[] args) {
        pair2Parser();
        pair3Parser();
        orParser();
        manyParser();
        eofParser();
        optionalParser();
        stopParser();
    }

    static void pair2Parser() {
        Parser<Tuple2<String, String>> catParser = Parser.string("Hello").pair2(Parser.string("World"));
        System.out.println("start pair2 parser");
        System.out.println(catParser.parse("HelloWorld"));
        System.out.println(catParser.parse("Hello, World"));
        System.out.println(catParser.parse("Hello World"));
    }

    static void pair3Parser() {
        Parser<Tuple3<String, String, String>> catParser = Parser.string("Hello").pair3(Parser.string(":"), Parser.string("World"));
        System.out.println("start pair3 parser");
        System.out.println(catParser.parse("Hello:World"));
        System.out.println(catParser.parse("Hello: World"));
        System.out.println(catParser.parse("Hello : World"));

        Parser p1 = new Pair3(Parser.string(" ").many(), Parser.string("Hello"), Parser.string(" ").many());
        Parser p2 = new Pair3(Parser.string(" ").many(), Parser.string("World"), Parser.string(" ").many());
        Parser parser = new Pair3(p1, Parser.string(":"), p2);
        Parser parser2 = new Pair3(Parser.string("{"), parser, Parser.string("}"));
        System.out.println(parser2.parse("{Hello:World}"));
        System.out.println(parser2.parse("{Hello: World}"));
        ParseResult parseResult = parser2.parse("{Hello : World}");
        System.out.println(parseResult);
    }

    static void orParser() {
        Parser<String> helloOrWorld = Parser.string("Hello").or(Parser.string("World"));
        Parser<Tuple2<String, String>> hw = helloOrWorld.pair2(helloOrWorld);
        System.out.println("start or parser");
        System.out.println(hw.parse("HelloWorld"));
        System.out.println(hw.parse("HelloHello"));
        System.out.println(hw.parse("WorldHello"));
        System.out.println(hw.parse("WorldWorld"));
    }

    static void manyParser() {
        Parser<List<String>> hellos = Parser.string("Hello").many();
        System.out.println("start many parser");
        System.out.println(hellos.parse(""));
        System.out.println(hellos.parse("Hello"));
        System.out.println(hellos.parse("HelloHello"));
        System.out.println(hellos.parse("HelloHelloHello"));
    }

    static void eofParser() {
        Parser<Tuple2<List<String>, String>> hellos = Parser.string("Hello").many().pair2(Parser.EOF());
        System.out.println("start eof parser");
        System.out.println(hellos.parse("Hello"));
        System.out.println(hellos.parse("Hello, World!"));
    }

    static void optionalParser() {
        Parser<Optional<List<String>>> hello = Parser.string("Hello").many().option();
        System.out.println("start optional parser");
        System.out.println(hello.parse(""));
        System.out.println(hello.parse("Hello"));
        System.out.println(hello.parse("HelloHello"));
        System.out.println(hello.parse("HelloHelloHello"));
    }

    static void stopParser() {
        Parser<String> stopParser = Parser.stop("World");
        System.out.println("start stop parser");
        System.out.println(stopParser.parse("Hello, World"));
        System.out.println(stopParser.parse("HelloWorld"));
        System.out.println(stopParser.parse("World Hello"));
    }
}
