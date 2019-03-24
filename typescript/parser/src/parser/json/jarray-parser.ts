import { JBool } from './type/jbool'
import { Parser } from '../common/parser'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import { JNull } from './type/jnull';
import { JArray } from './type/jarray';

class JArrayParser implements JParser<JArray> {
	sp: Parser<string[]> = P.many(P.string(" "))
	leftP = P.pair3(this.sp, P.string("["), this.sp)
	rightP = P.pair3(this.sp, P.string("]"), this.sp)

	parse(input: string): ParseResult<JArray> {
		let next = input
		next = this.sp.parse(next).next
		// const result = this.parser.parse(input)
		// if (result instanceof ParseSuccess) {
		// 	return new ParseSuccess(new JNull(), result.value)
		// } else if (result instanceof ParseFailer) {
		// 	return new ParseFailer(result.message, result.next)
		// }

		return new ParseFailer("", "")
	}
}
export const jArrayParser = new JArrayParser()
