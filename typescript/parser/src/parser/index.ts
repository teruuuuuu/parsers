import * as P from './common/parser'
import { ParseSuccess, ParseFailer } from './common/parser-result'
import { StringParser } from './common/string-parser'

function test(title:string, f:() => boolean) {
	console.info(title + ": start")
	if(f()){
		console.info(title + ": OK")
	} else {
		console.info(title + ": NG")
	}
}

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
