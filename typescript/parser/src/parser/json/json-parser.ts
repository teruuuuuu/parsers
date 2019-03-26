import { JString } from './type/jstrng'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import * as JP from './jparser'
import { JValue } from './type/jvalue';
import { JObjectParser } from './jobject-parser';
import { JArrayParser } from './jarray-parser';

export class JsonParser implements JParser<JValue> {

	parse(input: string): ParseResult<JValue> {
		const sp = P.many(P.string(" "))
		const v1Parser: P.Parser<JValue> = new JObjectParser()
		const v2Parser: P.Parser<JValue> = new JArrayParser()
		const parser: P.Parser<[[string[], JValue, string[]], string]> = P.pair2(P.pair3(sp, P.or(v1Parser, v2Parser), sp), P.eof())
		const result = parser.parse(input)
		if (result instanceof ParseSuccess) {
			return new ParseSuccess(result.value[0][1], result.next)
		} else if (result instanceof ParseFailer) {
			return new ParseFailer(result.message, result.next)
		}
	}
}
