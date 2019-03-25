import { Parser } from '../common/parser'
import { JParser } from './jparser'
import { ParseResult, ParseSuccess, ParseFailer } from '../common/parser-result'
import * as P from '../common/parser'
import { JNumber } from './type/jnumber'
import { Some, None } from '../common/type/option'
import { Option } from '../common/type/option';

export class JNumberParser implements JParser<JNumber> {
	signP: Parser<Option<string>> = P.option(P.or(P.string("+"), P.string("-")))
	baseP = P.or(P.string("E"), P.string("e"))
	numberP: Parser<string> = P.or(P.or(P.or(P.or(P.or(P.or(P.or(P.or(P.or(P.string("1"), P.string("2")), P.string("3")), P.string("4")),
		P.string("5")), P.string("6")), P.string("7")), P.string("8")), P.string("9")), P.string("9"))

	intP: Parser<string[]> = P.plus(this.numberP)
	floatP = P.option(P.pair2(P.string("."), this.intP))

	parse(input: string): ParseResult<JNumber> {
		let next = input
		let numberStr = ""
		const signResult = this.signP.parse(next)
		if (signResult instanceof ParseFailer) {
			return new ParseFailer(signResult.message, signResult.next)
		} else {
			next = signResult.next
			if (signResult.value instanceof Some) {
				numberStr += signResult.value.value
			}
		}
		const numberResult = this.intP.parse(next)
		if (numberResult instanceof ParseFailer) {
			return new ParseFailer(numberResult.message, numberResult.next)
		} else {
			next = numberResult.next
			numberResult.value.forEach(n => {
				numberStr += n
			})
		}
		const floatResult: ParseResult<Option<[string, string[]]>> = this.floatP.parse(next)
		if (floatResult instanceof ParseFailer) {
			return new ParseFailer(floatResult.message, floatResult.next)
		} else {
			next = floatResult.next
			if (floatResult.value instanceof Some) {
				numberStr += floatResult.value.value[0]
				floatResult.value.value[1].forEach(i => {
					numberStr += i
				})
			}
		}

		const baseResult: ParseResult<Option<[string, Option<string>, string[]]>> = P.option(P.pair3(this.baseP, this.signP, this.intP)).parse(next)
		if(baseResult instanceof ParseFailer) {
			return new ParseFailer(baseResult.message, baseResult.next)
		} else {
			next = baseResult.next
			if(baseResult.value instanceof Some) {
				numberStr += baseResult.value.value[0]
				if(baseResult.value.value[1] instanceof Some) {
					numberStr += baseResult.value.value[1].value
				}
				baseResult.value.value[2].forEach(i => {
					numberStr += i
				})
			}
		}
		return new ParseSuccess(new JNumber(Number(numberStr)), next)
	}
}
export const jNumberParser = new JNumberParser()
