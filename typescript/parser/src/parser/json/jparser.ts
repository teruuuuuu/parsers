import { ParseResult } from '../common/parser-result'
import { Parser } from '../common/parser'
import { JValue } from './type/jvalue'

export interface JParser<T extends JValue> extends Parser<T> { }
