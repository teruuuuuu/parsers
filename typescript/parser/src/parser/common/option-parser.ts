import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';
import { Option, Some, None } from './type/option'

export class OptionParser<T> implements Parser<Option<T>> {
	parser: Parser<T>
	constructor(parser: Parser<T>) {
		this.parser = parser
	}

	parse(input: string): ParseResult<Option<T>> {
		const result = this.parser.parse(input)
		if(result instanceof ParseSuccess) {
			return new ParseSuccess(new Some(result.value), result.next)
		} else {
			return new ParseSuccess(new None(), result.next)
		}
	}
}
