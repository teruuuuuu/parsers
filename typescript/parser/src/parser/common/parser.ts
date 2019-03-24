import {ParseResult, ParseFailer, ParseSuccess} from './parser-result';
import { ManyParser } from './many-parser';
import { Pair2Parser } from './pair2-parser';
import { Pair3Parser } from './pair3-parser';
import { PlusParser } from './plus-parser';
import { StringParser } from './string-parser';
import { StopWithEscape } from './stop-with-escape';
import { EofParser } from './eof-parser';
import { OrParser } from './or-parser';

export interface Parser<T> {
	parse(input: string ): ParseSuccess<T> | ParseFailer<T>;
}


export function string(literal: string): Parser<string> {
	return new StringParser(literal);
}
export function stopWithEscape(stop: string, escapes: Map<string, string>): Parser<string> {
	return new StopWithEscape(stop, escapes);
}
export function pair2<T,U>(lps: Parser<T>, rps: Parser<U>): Parser<(T | U)[]> {
	return new Pair2Parser(lps, rps)
}
export function pair3<T,U,V>(lps: Parser<T>, cps: Parser<U>, rps: Parser<V>): Parser<(T | U | V)[]> {
	return new Pair3Parser(lps, cps, rps)
}
export function many<T>(parser: Parser<T>): Parser<T[]> {
	return new ManyParser(parser)
}
export function plus<T>(parser: Parser<T>): Parser<T[]> {
	return new PlusParser(parser)
}
export function or<T>(parser1: Parser<T>, parser2: Parser<T>): Parser<T> {
	return new OrParser(parser1, parser2)
}
export function eof(): Parser<string> {
	return new EofParser()
}
