import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class OrParser<T> implements Parser<T> {
	parser1: Parser<T>;
	parser2: Parser<T>;
	public constructor(parser1: Parser<T>, parser2: Parser<T>) {
		this.parser1 = parser1;
		this.parser2 = parser2;
	}

	parse(input: string): ParseResult<T> {
		const result1 = this.parser1.parse(input)
		if(result1 instanceof ParseSuccess) {
			return new ParseSuccess(result1.value, result1.next)
		} else if(result1 instanceof ParseFailer) {
			const result2 = this.parser2.parse(input)
			if(result2 instanceof ParseSuccess) {
				return new ParseSuccess(result2.value, result2.next)
			} else if(result2 instanceof ParseFailer) {
				return new ParseFailer(result1.message + result2.message, result1.next)
			}
		}
	}
}
