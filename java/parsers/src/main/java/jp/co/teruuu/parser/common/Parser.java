package jp.co.teruuu.parser.common;

import jp.co.teruuu.parser.common.type.Tuple2;
import jp.co.teruuu.parser.common.type.Tuple3;
import jp.co.teruuu.parser.json.JsonParser;
import jp.co.teruuu.parser.json.type.JValue;

import java.util.List;
import java.util.Optional;

/**
 * パーサー用インターフェース
 * @param <T> パース結果の型
 */
public interface Parser<T> {
    /**
     * パースする
     * @param input
     * @return
     */
    ParseResult<T> parse(String input);

    default Parser<T> or(Parser<T> rhs) {
        return new Or<>(this, rhs);
    }
    default <U> Parser<Tuple2<T, U>> pair2(Parser<U> rhs) {
        return new Pair2<>(this, rhs);
    }
    default <U, V> Parser<Tuple3<T, U, V>> pair3(Parser<U> shs, Parser<V> ths) {
        return new Pair3<>(this, shs, ths);
    }
    static Parser<String> string(String literal) {
        return new StringParser(literal);
    }
    default Parser<List<T>> many() {
        return new ManyParser<T>(this);
    }
    default Parser<List<T>> plus() {
        return new PlusParser<>(this);
    }
    static Parser<String> EOF() {
        return new EOFParser();
    }
    default Parser<Optional<T>> option() {
        return new OptionParser<>(this);
    }
    static Parser<String> stop(String literal) {
        return new StopWordParser(literal);
    }
    static Parser<String> stopWithEscape(String stop, List<Tuple2<String, String>> escapes) {
        return new StopWithEscape(stop, escapes);
    }
}