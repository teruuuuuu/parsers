import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class Pair2Parser<T,U> implements Parser<[T, U]> {
	lp: Parser<T>;
	rp: Parser<U>;
	constructor(lp: Parser<T>, rp: Parser<U>) {
		this.lp = lp;
		this.rp = rp;
	}

	parse(input: string): ParseResult<[T, U]> {
		const lResult = this.lp.parse(input)
		if(lResult instanceof ParseSuccess) {
			const rResult = this.rp.parse(lResult.next)
			if(rResult instanceof ParseSuccess) {
				return new ParseSuccess<[T,U]>([lResult.value, rResult.value], rResult.next)
			} else if(rResult instanceof ParseFailer) {
				return new ParseFailer(rResult.message, rResult.next)
			}
		} else if(lResult instanceof ParseFailer) {
			return new ParseFailer(lResult.message, lResult.next)
		}
	}
}
