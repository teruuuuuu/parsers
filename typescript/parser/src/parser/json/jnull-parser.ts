import { JBool } from './type/jbool'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import { JNull } from './type/jnull';

class JNullParser implements JParser<JNull> {
	parser = P.string("null")

	parse(input: string): ParseResult<JNull> {
		const result = this.parser.parse(input)
		if (result instanceof ParseSuccess) {
			return new ParseSuccess(new JNull(), result.value)
		} else if (result instanceof ParseFailer) {
			return new ParseFailer(result.message, result.next)
		}
	}
}
export const jNullParser = new JNullParser()
