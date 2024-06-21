package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Either;
import jp.co.teruuu.common.Tuple;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/**
 * パーサー用インターフェース
 * @param <T> パース結果の型
 */
public interface Parser<T> {

    ParseResult<T> parse(String input, int location);

    default ParseResult<T> parse(String input) {
        return parse(input, 0);
    }

    default  <X>Parser<Tuple<T, X>> and(Parser<X> parser) {
        return new AndParser<>(this, parser);
    }

    default  <X>Parser<T> andLeft(Parser<X> parser) {
        return this.and(parser).map(Tuple::fst);
    }

    default  <X>Parser<X> andRight(Parser<X> parser) {
        return this.and(parser).map(Tuple::snd);
    }

    default  <X>Parser<Void> andVoid(Parser<X> parser) {
        return this.and(parser).map(value -> null);
    }

    default  <X,Y,Z>Parser<List<T>> array(Parser<X> lbrackets, Parser<Y> rbrackets, Parser<Z> separtor) {
        return new ArrayParser<>(this, lbrackets, rbrackets, separtor);
    }

    static Parser<String> charP(char c) {
        return new CharParser(c);
    }

    static Parser<String> dquoteString() {
        return new DQuoteStringParser();
    }

    static <X,Y>Parser<Either<X,Y>> either(Parser<X> fparser, Parser<Y> sparser) {
        return new EitherParser<>(fparser, sparser);
    }

    default  <X>Parser<Either<T, X>> either(Parser<X> parser) {
        return new EitherParser<>(this, parser);
    }

    static Parser<Void> end() {
        return new EndParser();
    }

    static Parser<String> escape() {
        return new EscapeParser();
    }

    default <X> Parser<X> map(Function<T,X> mapFunc) {
        return (input, location) -> {
            switch (this.parse(input, location)) {
                case ParseResult.Success<T> success -> {
                    return new ParseResult.Success<>(mapFunc.apply(success.value()), success.next());
                }
                case ParseResult.Failure<T> failure -> {
                    return new ParseResult.Failure<>(failure.message(), failure.next());
                }
            }
        };
    }

    static <X> Parser<String> not(Parser<X> parser) {
        return new NotParser<>(parser);
    }
    default Parser<String> not() {
        return new NotParser<>(this);
    }

    static Parser<Either<Integer,Double>> number() {
        return new NumberParser();
    }

    static <X>Parser<Optional<X>> optional(Parser<X> parser) {
        return new OptionalParser<>(parser);
    }

    default Parser<Optional<T>> optional() {
        return new OptionalParser<>(this);
    }

    static <X>Parser<X> or(Parser<X> fparser, Parser<X> sparser) {
        return new OrParser<>(fparser, sparser);
    }

    default  Parser<T> or(Parser<T> parser) {
        return new OrParser<>(this, parser);
    }

    static <X>Parser<List<X>> seq(Parser<X> parser) {
        return new SeqParser<>(parser);
    }

    default Parser<List<T>> seq() {
        return new SeqParser<>(this);
    }

    static Parser<Void> skip(String literal) {
        return new SkipParser(literal);
    }

    static Parser<Void> skip(char c) {
        return new SkipCharParser(c);
    }

    static Parser<String> stopWord(String literal) {
        return new StopWardParser(literal);
    }

    static Parser<String> string(String literal) {
        return new StringParser(literal);
    }

    default Parser<T> withSkipSpace() {
        return new SkipSpaceParser().andRight(this);
    }
}