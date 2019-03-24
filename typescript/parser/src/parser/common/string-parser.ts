import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class StringParser implements Parser<string> {
	literal: string;
	constructor(literal: string) {
		// super()
		this.literal = literal;
	}

	parse(input: string): ParseSuccess<string> | ParseFailer<string> {
		if (input.startsWith(this.literal)) {
			return new ParseSuccess(this.literal, input.substring(this.literal.length))
		}
		return new ParseFailer("expect: " + this.literal, input);
	}
}
