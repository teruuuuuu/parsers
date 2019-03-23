import { Parser } from './parser';
import { ParseResult, ParseSuccess, ParseFailer } from './parser-result';

export class StopWithEscape implements Parser<string> {
	stop: string;
	escapes: Map<string, string>;
	constructor(stop: string, escapes: Map<string, string>) {
		this.stop = stop;
		this.escapes = escapes;
	}

	parse(input: string): ParseResult<String> {
		let next = input
		let result = ""
		let continueFlg = true
		while(continueFlg) {
			this.escapes.forEach((value,key) => {
				if(next.startsWith(key)) {
					result += value
					next = next.substring(key.length)
				}
			})
			if(next.length > 0 && !next.startsWith(this.stop)) {
				result += next.substring(0, 1)
				next = next.substring(1)
			} else {
				continueFlg = false
			}
		}
		return new ParseSuccess(result, next);
	}
}
