import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class PlusParser<T> implements Parser<T[]> {
	parser: Parser<T>;
	public constructor(parser: Parser<T>) {
		this.parser = parser;
	}

	parse(input: string): ParseResult<T[]> {
		let continueFlg = true
		let next = input
		let result = new Array<T>()
		let parseResult: ParseResult<T>
		while(continueFlg) {
			parseResult = this.parser.parse(next)
			if(parseResult instanceof ParseSuccess) {
				result.push(parseResult.value)
				next = parseResult.next
			} else if(parseResult instanceof ParseFailer) {
				continueFlg = false
			}
		}
		if(result.length == 0 && parseResult instanceof ParseFailer) {
			return new ParseFailer(parseResult.message, next)
		} else {
			return new ParseSuccess(result, next)
		}
	}
}
