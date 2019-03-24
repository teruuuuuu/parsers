import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class ManyParser<T> implements Parser<T[]> {
	parser: Parser<T>;
	constructor(parser: Parser<T>) {
		this.parser = parser;
	}

	parse(input: string): ParseSuccess<T[]> | ParseFailer<T[]> {
		let continueFlg = true
		let next = input
		let result = new Array<T>()
		while (continueFlg) {
			const parseResult = this.parser.parse(next)
			if (parseResult instanceof ParseSuccess) {
				result.push(parseResult.value)
				next = parseResult.next
			} else if (parseResult instanceof ParseFailer) {
				continueFlg = false
			}
		}
		return new ParseSuccess(result, next)
	}
}
