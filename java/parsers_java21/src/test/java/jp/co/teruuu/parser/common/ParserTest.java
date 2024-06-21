package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Either;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ParserTest {

    @Test
    public void test() {
        ParseResult<String> parseResult1 = Parser.dquoteString().parse("\"abc\\\"def\"");
        assertTrue(parseResult1 instanceof ParseResult.Success<String>);
        ParseResult.Success<String> parseSuccess1 = (ParseResult.Success<String>) parseResult1;
        assertEquals(parseSuccess1.value(), "abc\"def");

        Parser<List<String>> arrayParser = Parser.dquoteString().array(Parser.charP('['), Parser.charP(']'), Parser.charP(','));
        arrayParser.parse("[ \"abc\" ,\"def\"]");

        NumberParser numberParser = new NumberParser();
        ParseResult<Either<Integer,Double>> numParseResult = numberParser.parse("1234567890");
        assertTrue(numParseResult instanceof ParseResult.Success<Either<Integer,Double>>);
        assertEquals(((Either.Left) ((ParseResult.Success<Either<Integer, Double>>) numParseResult).value()).x(), 1234567890);
        numParseResult = numberParser.parse("0.12345");
        assertTrue(numParseResult instanceof ParseResult.Success<Either<Integer,Double>>);
        assertEquals(((Either.Right) ((ParseResult.Success<Either<Integer, Double>>) numParseResult).value()).y(), 0.12345);
        numParseResult = numberParser.parse("-1234567890");
        assertTrue(numParseResult instanceof ParseResult.Success<Either<Integer,Double>>);
        assertEquals(((Either.Left) ((ParseResult.Success<Either<Integer, Double>>) numParseResult).value()).x(), -1234567890);
        numParseResult = numberParser.parse("+1234567890.");
        assertTrue(numParseResult instanceof ParseResult.Success<Either<Integer,Double>>);
        assertEquals(((Either.Right) ((ParseResult.Success<Either<Integer, Double>>) numParseResult).value()).y(), 1234567890.);
        numParseResult = numberParser.parse("+1234567890.23456");
        assertTrue(numParseResult instanceof ParseResult.Success<Either<Integer,Double>>);
        assertEquals(((Either.Right) ((ParseResult.Success<Either<Integer, Double>>) numParseResult).value()).y(), 1234567890.23456);
    }
}
