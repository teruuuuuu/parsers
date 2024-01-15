package jp.co.teruuu.parser.common;

import jp.co.teruuu.common.Either;

public class NumberParser implements Parser<Either<Integer, Double>> {
    int reallyBig = Integer.MAX_VALUE / 10;
    double reallyMini = Double.MAX_VALUE / 10;

    @Override
    public ParseResult<Either<Integer, Double>> parse(String input, int location) {
        int signe = 1;
        int number = 0;
        double decimal = 0;
        double decimalDigit = 1;
        if (input.charAt(location) == '-') {
            signe = -1;
            location++;
        } else if (input.charAt(location) == '+') {
            signe = 1;
            location++;
        }

        if (input.charAt(location) == '0') {
            number = 0;
            location++;
        } else {
            if (input.charAt(location) >= '1' && input.charAt(location) <= '9') {
                number += (input.charAt(location) - '0');
                location++;
                while ((input.charAt(location) >= '0' && input.charAt(location) <= '9')) {
                    if (number >= reallyBig) {
                        return new ParseResult.Failure<>(String.format("exceeds the limit(location=[%d] input=[%s]", location, input), location);
                    }
                    number *= 10;
                    number += (input.charAt(location) - '0');
                    if (location == input.length() - 1) {
                        break;
                    } else {
                        location++;
                    }
                }
            } else {
                return new ParseResult.Failure<>(String.format("not number(location=[%d] input=[%s]", location, input), location);
            }
        }
        if (input.charAt(location) != '.') {
            return new ParseResult.Success<>(new Either.Left<>(signe * number), location);
        } else {
            if (location == input.length() - 1) {
                return new ParseResult.Success<>(new Either.Right<>((double)signe * number), location);
            }
            location++;
            while ((input.charAt(location) >= '0' && input.charAt(location) <= '9')) {
                if (decimalDigit >= reallyMini) {
                    return new ParseResult.Failure<>(String.format("exceeds the limit(location=[%d] input=[%s]", location, input), location);
                }
                decimal *= 10;
                decimal += (input.charAt(location) - '0');
                decimalDigit *= 10;
                if (location == input.length() - 1) {
                    break;
                } else {
                    location++;
                }
            }
            decimal = decimal / decimalDigit;
            return new ParseResult.Success<>(new Either.Right<>(signe * ((double)number + decimal)), location);

        }
    }
}
