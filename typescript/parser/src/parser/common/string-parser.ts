import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class StringParser implements Parser<string> {
	literal: string;
	constructor(literal: string) {
		this.literal = literal;
	}

	parse(input: string): ParseResult<string> {
		if (input.startsWith(this.literal)) {
			return new ParseSuccess(this.literal, input.substring(this.literal.length))
		}
		return new ParseFailer("expect: " + this.literal, input);
	}
}
