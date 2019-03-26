import { Parser } from '../common/parser'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import * as JP from './jparser'
import { JArray } from './type/jarray';
import { JValue } from './type/jvalue';
import { Option, Some } from '../common/type/option';

export class JArrayParser implements JParser<JArray> {

	parse(input: string): ParseResult<JArray> {
		let values: JValue[] = []
		const sp: Parser < string[] > = P.many(P.string(" "))
		const leftP = P.pair3(sp, P.string("["), sp)
		const rightP = P.pair3(sp, P.string("]"), sp)

		let next = input
		const leftResult = leftP.parse(next)
		if (leftResult instanceof ParseFailer) {
			return new ParseFailer(leftResult.message, leftResult.next)
		}
		next = leftResult.next

		const jvp: JParser<JValue>  = JP.values()
		const valueP: Parser<[string[], JValue, string[]]> = P.pair3(sp, jvp, sp)
		const valuesP: Parser<[[string[], string, string[]], [string[], JValue, string[]]][]> = P.many(P.pair2(P.pair3(sp, P.string(","), sp), valueP))

		const valueResult: ParseResult<Option<[string[], JValue, string[]]>> = P.option(valueP).parse(next)
		if(valueResult instanceof ParseFailer) {
			return new ParseFailer(valueResult.message, valueResult.next)
		} else {
			next = valueResult.next
			if(valueResult.value instanceof Some) {
				values.push(valueResult.value.value[1])
				const valuesResult: ParseResult<[[string[], string, string[]], [string[], JValue, string[]]][]> = valuesP.parse(next)
				if(valuesResult instanceof ParseFailer) {
					return new ParseFailer(valuesResult.message, valuesResult.next)
				}
				next = valuesResult.next
				valuesResult.value.forEach(v => {
					values.push(v[1][1])
				})
			}
		}
		const rightResult: ParseResult<Option<[[string[], JValue, string[]], [[string[], string, string[]], [string[], JValue, string[]]][]]>> = rightP.parse(next)
		if (rightResult instanceof ParseFailer) {
			return new ParseFailer(rightResult.message, rightResult.next)
		}
		next = rightResult.next
		return new ParseSuccess(new JArray(values), next)
	}
}
