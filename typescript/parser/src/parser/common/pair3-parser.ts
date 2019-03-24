import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class Pair3Parser<T,U,V> implements Parser<[T,U,V]> {
	lp: Parser<T>;
	cp: Parser<U>;
	rp: Parser<V>;
	public constructor(lp: Parser<T>, cp: Parser<U>, rp: Parser<V>) {
		this.lp = lp;
		this.cp = cp;
		this.rp = rp;
	}

	parse(input: string): ParseSuccess<[T,U,V]> | ParseFailer<[T,U,V]> {
		const lResult = this.lp.parse(input)
		if(lResult instanceof ParseSuccess) {
			const cResult = this.cp.parse(lResult.next)
			if(cResult instanceof ParseSuccess) {
				const rResult = this.rp.parse(cResult.next)
				if(rResult instanceof ParseSuccess) {
					return new ParseSuccess<[T,U,V]>([lResult.value, cResult.value, rResult.value], rResult.next)
				} else if(rResult instanceof ParseFailer) {
					return new ParseFailer(rResult.message, rResult.next)
				}
			} else if(cResult instanceof ParseFailer) {
				return new ParseFailer(cResult.message, cResult.next)
			}
		} else if(lResult instanceof ParseFailer) {
			return new ParseFailer(lResult.message, lResult.next)
		}
	}
}
