import { JBool } from './type/jbool'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'

class JBoolParser implements JParser<JBool> {
	parser = P.or(P.string("true"), P.string("false"))

	parse(input: string): ParseResult<JBool> {
		const result = this.parser.parse(input)
		if (result instanceof ParseSuccess) {
			if(result.value == "true") {
				return new ParseSuccess(new JBool(true), result.value)
			} else {
				return new ParseSuccess(new JBool(false), result.value)
			}
		} else if (result instanceof ParseFailer) {
			return new ParseFailer(result.message, result.next)
		}
	}
}
export const jBoolParser = new JBoolParser()
