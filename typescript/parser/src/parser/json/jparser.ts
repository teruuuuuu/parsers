import { ParseResult } from '../common/parser-result'
import { Parser } from '../common/parser'
import { JValue } from './type/jvalue'
import { JString } from './type/jstrng'
// import { jStringParser } from './jstring-parser';

export interface JParser<T extends JValue> extends Parser<T> {
	// jstring(): JParser<JString> {
	// 	return jStringParser
	// }
}

// export const jparser = new JParser()
