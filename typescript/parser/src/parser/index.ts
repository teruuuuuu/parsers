import { ParseFailer, ParseSuccess, ParseResult } from './common/parser-result';
import * as P  from './common/parser';
import { JString } from './json/type/jstrng'
import { jStringParser } from './json/jstring-parser'
import { Option, Some, None } from './common/type/option';
import { JArrayParser } from './json/jarray-parser';
import { jNumberParser } from './json/jnumber-parser';
import { JNumber } from './json/type/jnumber';
import { jBoolParser } from './json/jbool-parser';
import { JBool } from './json/type/jbool';
import { jNullParser } from './json/jnull-parser';
import { JNull } from './json/type/jnull';
import { JObjectParser } from './json/jobject-parser';
import { JObject } from './json/type/jobject';
import { JArray } from './json/type/jarray';
import { JValue } from './json/type/jvalue';
import { JsonParser } from './json/json-parser';


function test(title: string, f: () => boolean) {
	console.log("%c" + title + ": start", "color: green");
	if (f()) {
		console.log("%c " + title + ": OK", "color: green");
	} else {
		console.error(" " + title + ": NG")
	}
}
console.time('all');

function stringParserTest(): boolean {
	const parser = P.string("Hello");
	const result1 = parser.parse("Hello, world");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value == "Hello")) { return false; }

	const result2 = parser.parse("hello, world");
	if (!(result2 instanceof ParseFailer)) { return false; }
	return true
}
test("StringParseSpec", stringParserTest)

function stopWithEscapeTest(): boolean {
	const parser = P.stopWithEscape("\"", new Map([["\\\"", "\""]]))
	const result1 = parser.parse("Hello, world");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value == "Hello, world")) { return false; }

	const result2 = parser.parse("\"Hello, world\"");
	if (!(result2 instanceof ParseSuccess)) { return false; }
	if (!(result2.value == "")) { return false; }

	const result3 = parser.parse("\\\"Hello, world\\\"");
	if (!(result3 instanceof ParseSuccess)) { return false; }
	if (!(result3.value == "\"Hello, world\"")) { return false; }

	return true
}
test("StopWithEscapeSpec", stopWithEscapeTest)

function pair2ParserTest(): boolean {
	const parser = P.pair2(P.string("Hello"), P.string("World"))
	const result1 = parser.parse("HelloWorld");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value[0] == "Hello" && result1.value[1] == "World")) { return false; }

	const result2 = parser.parse("Hello,World");
	if (!(result2 instanceof ParseFailer)) { return false; }

	const result3 = parser.parse("hello,World");
	if (!(result3 instanceof ParseFailer)) { return false; }
	return true
}
test("Pair2ParseSpec", pair2ParserTest)

function pair3ParserTest(): boolean {
	const parser = P.pair3(P.string("Hello"), P.string(","), P.string("World"))
	const result1 = parser.parse("Hello,World");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value[0] == "Hello" && result1.value[1] == "," && result1.value[2] == "World")) { return false; }

	const result2 = parser.parse("Hello,Worl");
	if (!(result2 instanceof ParseFailer)) { return false; }

	const result3 = parser.parse("HelloWorld");
	if (!(result3 instanceof ParseFailer)) { return false; }
	return true
}
test("Pair3ParseSpec", pair3ParserTest)

function manyParserTest(): boolean {
	const parser = P.many(P.string("Hello"))
	const result1 = parser.parse("HelloHellohello");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value[0] == "Hello" && result1.value[1] == "Hello")) { return false; }

	const result2 = parser.parse("hello,Worl");
	if (!(result2 instanceof ParseSuccess)) { return false; }
	if (!(result2.value.length == 0)) { return false; }
	return true
}
test("ManyParserSpec", manyParserTest)

function plusParserTest(): boolean {
	const parser = P.plus(P.string("Hello"))
	const result1 = parser.parse("HelloHellohello");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value[0] == "Hello" && result1.value[1] == "Hello")) { return false; }

	const result2 = parser.parse("hello,Worl");
	if (!(result2 instanceof ParseFailer)) { return false; }
	return true
}
test("PlusParserSpec", plusParserTest)

function eofParserTest(): boolean {
	const parser = P.pair2(P.string("Hello"), P.eof())
	const result1 = parser.parse("Hello");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value[1] == "")) { return false; }
	return true
}
test("EofParserSpec", eofParserTest)

function orParserTest(): boolean {
	const parser = P.or(P.string("Hello"), P.string("World"))
	const result1 = parser.parse("Hello");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value == "Hello")) { return false; }
	return true
}
test("OrParserSpec", orParserTest)

function optionParserTest(): boolean {
	const parser = P.option(P.string("Hello"))

	const result1: ParseResult<Option<string>> = parser.parse("Hello");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value instanceof Some)) { return false; }
	if (!(result1.value.value == "Hello")) { return false; }

	const result2: ParseResult<Option<string>> = parser.parse("hello");
	if (!(result2 instanceof ParseSuccess)) { return false; }
	if (!(result2.value instanceof None)) { return false; }

	return true
}
test("OptionParserSpec", optionParserTest)


function jStringParserTest(): boolean {
	const parser = jStringParser
	const result1 = parser.parse("\"Hello\"");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value.equals(new JString("Hello")))) { return false; }

	const result2 = parser.parse("\"Hello\", World\"");
	if (!(result2 instanceof ParseSuccess)) { return false; }
	if (!(result2.value.equals(new JString("Hello")))) { return false; }

	const result3 = parser.parse("Hello\", World");
	if (!(result3 instanceof ParseFailer)) { return false; }

	const result4 = parser.parse("\"Hello\\\", World\"");
	if (!(result4 instanceof ParseSuccess)) { return false; }
	if (!(result4.value.equals(new JString("Hello\", World")))) { return false; }
	return true
}
test("JStringParserSpec", jStringParserTest)

function jNumberParserTest(): boolean {
	const parser = jNumberParser
	const result1 = parser.parse("+123.456E-123");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value.equals(new JNumber(Number("+123.456E-123"))))) { return false; }

	return true
}
test("JNumberParserSpec", jNumberParserTest)

function jBoolParserTest(): boolean {
	const parser = jBoolParser
	const result1 = parser.parse("true");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value.equals(new JBool(true)))) { return false; }

	const result2 = parser.parse("false");
	if (!(result2 instanceof ParseSuccess)) { return false; }
	if (!(result2.value.equals(new JBool(false)))) { return false; }

	const result3 = parser.parse("fals");
	if (!(result3 instanceof ParseFailer)) { return false; }
	return true
}
test("JBoolParserSpec", jBoolParserTest)

function jNullParserTest(): boolean {
	const parser = jNullParser
	const result1 = parser.parse("null");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	if (!(result1.value.equals(new JNull()))) { return false; }

	const result2 = parser.parse("nul");
	if (!(result2 instanceof ParseFailer)) { return false; }

	return true
}
test("JNullParserSpec", jNullParserTest)

function jArrayParserTest(): boolean {
	const parser = new JArrayParser()
	const result1 = parser.parse("[ 1 , \"2\" , [ true, null, [] ]] ");
	if (!(result1 instanceof ParseSuccess)) { return false; }

	return true
}
test("JArrayParserSpec", jArrayParserTest)

function jObjectParserTest(): boolean {
	const parser = new JObjectParser()
	const result1 = parser.parse("{\"a\": 1, \"c\": [{\"d\": 345}]}");
	if (!(result1 instanceof ParseSuccess)) { return false; }

	const value1:[JString, JValue] = [new JString("a"), new JNumber(1)]
	const value2:[JString, JValue] = [new JString("c"), new JArray(new Array(new JObject(new Map([[new JString("d"), new JNumber(345)]]))))];
	const ans1 = new JObject(new Map([value1, value2]))
	if (!(result1.value.equals(ans1))) { return false; }
	return true
}
test("JobjectParserSpec", jObjectParserTest)

function jsonParserTest(): boolean {
	const parser = new JsonParser()
	const result1 = parser.parse(" {\"a\": [ [1, 2, 3,4], {} ]} ");
	if (!(result1 instanceof ParseSuccess)) { return false; }
	const v1: JValue = new JObject(new Map())
	const v2: JValue = new JArray(new Array(new JNumber(1), new JNumber(2), new JNumber(3), new JNumber(4)))
	const ans1 = new JObject(new Map([[new JString("a"), new JArray(new Array(v2, v1))]]))
	if (!(result1.value.equals(ans1))) { return false; }

	const result2 = parser.parse("  {\"a\": [ [1, 2, 3,4], {} ]}, {}");
	if (!(result2 instanceof ParseFailer)) { return false; }
	return true
}
test("JsonParserSpec", jsonParserTest)


console.timeEnd('all');
