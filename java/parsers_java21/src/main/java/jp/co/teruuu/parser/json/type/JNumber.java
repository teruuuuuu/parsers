package jp.co.teruuu.parser.json.type;

import jp.co.teruuu.common.Either;

public record JNumber(Either<Integer, Double> number) implements Json {
}
