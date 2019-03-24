import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class EofParser implements Parser<string> {
	parse(input: string): ParseResult<string> {
		if(input == "") {
			return new ParseSuccess("", "")
		} else {
			return new ParseFailer("expect: eof", input)
		}
	}
}


