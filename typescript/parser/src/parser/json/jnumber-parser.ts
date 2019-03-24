import { JString } from './type/jstrng'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import { JNumber } from './type/jnumber';

class JNumberParser implements JParser<JNumber> {
	signP = P.or(P.string("+"), P.string("-"))
	parser = P.pair3(P.string("\""), P.stopWithEscape("\"", new Map([["\\\"", "\""]])), P.string("\""))

	parse(input: string): ParseSuccess<JNumber> | ParseFailer<JNumber>{
		// const result = this.parser.parse(input)
		// if (result instanceof ParseSuccess) {
		// 	return new ParseSuccess(new JString(result.value[1]), result.next)
		// } else if (result instanceof ParseFailer) {
		// 	return new ParseFailer(result.message, result.next)
		// }
		return null
	}
}
export const jNumberParser = new JNumberParser()
