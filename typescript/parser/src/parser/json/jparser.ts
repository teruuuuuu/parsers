import { Parser } from '../common/parser'
import { JValue } from './type/jvalue'
import * as P from '../common/parser'
import { JNullParser } from './jnull-parser';
import { JBoolParser } from './jbool-parser';
import { JStringParser } from './jstring-parser';
import { JNumberParser } from './jnumber-parser';
import { JArrayParser } from './jarray-parser';
import { JObjectParser } from './jobject-parser';

export interface JParser<T extends JValue> extends Parser<T> { }

export function values(): JParser<JValue>  {
	return P.or(P.or(P.or(P.or(P.or(new JNullParser(), new JBoolParser()), new JStringParser()), new JNumberParser()), new JArrayParser()), new JObjectParser())
}
