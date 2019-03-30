package json

import (
	"fmt"
	"unsafe"
)

const (
	JNull int8 = iota
	JBool
	JString
	JNumber
	JArray
	JObject
)

type JValue struct {
	Vtype int8
	Text  []byte
	Nest  []JValue
}

type JParseResult struct {
	Result  bool
	Next    []byte
	Value   *JValue
	Message string
}

type JParser interface {
	Parse(input []byte) *JParseResult
}

func (jv JValue) Format() []string {
	var ret = []string{}
	if jv.Vtype == JNull || jv.Vtype == JBool || jv.Vtype == JNumber {
		ret = append(ret, *(*string)(unsafe.Pointer(&jv.Text)))
	} else if jv.Vtype == JString {
		ret = append(ret, "\""+*(*string)(unsafe.Pointer(&jv.Text))+"\"")
	} else if jv.Vtype == JArray {
		ret = append(ret, "[")
		for i := range jv.Nest {
			nestf := jv.Nest[i].Format()
			for j := range nestf {
				var line = "  " + nestf[j]
				if i != len(jv.Nest)-1 && j == len(nestf)-1 {
					line = line + ","
				}
				ret = append(ret, line)
			}
		}
		ret = append(ret, "]")
	} else if jv.Vtype == JObject {
		ret = append(ret, "{")
		for i := 0; i < len(jv.Nest)/2; i++ {
			nestf := jv.Nest[2*i+1].Format()
			for j := range nestf {
				var line = "  "
				if j == 0 {
					line = line + jv.Nest[2*i].Format()[0] + ": "
				}
				line = line + nestf[j]
				if i != len(jv.Nest)/2-1 && j == len(nestf)-1 {
					line = line + ","
				}
				ret = append(ret, line)
			}
		}
		ret = append(ret, "}")
	}
	return ret
}

func (result JParseResult) Show() {
	if result.Result {
		fmt.Println("parse: OK")
		result.Value.Show()
		fmt.Println("next:" + *(*string)(unsafe.Pointer(&result.Next)))
	} else {
		fmt.Println("parse: NG")
		fmt.Println("message:" + *(*string)(unsafe.Pointer(&result.Message)))
	}
}

func (jValue JValue) Show() {
	fmt.Print("Value type: ")
	if jValue.Vtype == JNull {
		fmt.Println("JNull")
	} else if jValue.Vtype == JBool {
		fmt.Println("JBool")
	} else if jValue.Vtype == JString {
		fmt.Println("JString")
	} else if jValue.Vtype == JNumber {
		fmt.Println("JNumber")
	} else if jValue.Vtype == JArray {
		fmt.Println("JArray")
	} else if jValue.Vtype == JObject {
		fmt.Println("JObject")
	}
	fmt.Println("text:" + *(*string)(unsafe.Pointer(&jValue.Text)))
}

type JNullParser struct{}
type JBoolParser struct{}
type JStringParser struct{}
type JNumberParser struct{}
type JArrayParser struct{}
type JObjectParser struct{}
type JsonParser struct{}

func (parser JNullParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	_, next := spSeq(input)
	var isParseOk, text, nexts = scrap(next, []byte("null"))
	if isParseOk {
		result.Result = true
		var value = JValue{}
		value.Vtype = JNull
		value.Text = text
		result.Value = &value
		_, result.Next = spSeq(nexts)
		return &result
	} else {
		result.Result = false
		result.Message = "expect: null\n" + "actually: " + *(*string)(unsafe.Pointer(&input))
		return &result
	}
}

func (parser JBoolParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	_, next := spSeq(input)
	var isParseOk, text, nexts = scraps(next, [][]byte{[]byte("true"), []byte("false")})
	if isParseOk {
		result.Result = true
		var value = JValue{}
		value.Vtype = JBool
		value.Text = text
		result.Value = &value
		_, result.Next = spSeq(nexts)
		return &result
	} else {
		result.Result = false
		return &result
	}
}

func (parser JStringParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	_, next := spSeq(input)
	var doubleQoute = []byte("\"")
	var escapes = [][][]byte{{[]byte("\\\""), []byte("\"")}}
	var ret = []byte("")
	if startsWith(next, doubleQoute) {
		next = next[len(doubleQoute):]
		for {
			for i := range escapes {
				if startsWith(next, escapes[i][0]) {
					next = next[len(escapes[i][0]):]
					ret = append(ret, escapes[i][1]...)
				}
			}
			if len(next) == 0 || startsWith(next, doubleQoute) {
				break
			} else {
				ret = append(ret, next[0])
				next = next[1:]
			}
		}
		if startsWith(next, doubleQoute) {
			next = next[len(doubleQoute):]
			var jValue = JValue{}
			jValue.Vtype = JString
			jValue.Text = ret
			result.Result = true
			result.Value = &jValue
			_, next := spSeq(next)
			result.Next = next
			return &result
		} else {
			result.Result = false
			return &result
		}
	} else {
		result.Result = false
		return &result
	}
}

func (parser JNumberParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	var ret = []byte("")

	_, next := spSeq(input)
	var plus = []byte("+")
	var minus = []byte("-")
	var numbers = [][]byte{[]byte("0"), []byte("1"), []byte("2"), []byte("3"), []byte("4"),
		[]byte("5"), []byte("6"), []byte("7"), []byte("8"), []byte("9")}
	var piriod = []byte(".")
	var e = []byte("e")
	var E = []byte("E")

	////////////正負の判定//////////
	if startsWith(next, plus) {
		ret = append(ret, plus...)
		next = next[len(plus):]
	} else if startsWith(next, minus) {
		ret = append(ret, minus...)
		next = next[len(minus):]
	}
	////////////整数部分の判定//////////
	var intNumJudge = false
	for len(next) > 0 {
		var findFlg = false
		for i := range numbers {
			if startsWith(next, numbers[i]) {
				if !intNumJudge {
					intNumJudge = true
				}
				findFlg = true
				ret = append(ret, numbers[i]...)
				next = next[len(numbers[i]):]
			}
		}
		if !findFlg {
			break
		}
	}
	if !intNumJudge {
		result.Result = false
		return &result
	}

	////////////小数部分の判定//////////
	if startsWith(next, piriod) {
		ret = append(ret, piriod...)
		next = next[len(piriod):]
		var floadJudge = false
		for len(next) > 0 {
			var findFlg = false
			for i := range numbers {
				if startsWith(next, numbers[i]) {
					if !floadJudge {
						floadJudge = true
					}
					findFlg = true
					ret = append(ret, numbers[i]...)
					next = next[len(numbers[i]):]
				}
			}
			if !findFlg {
				break
			}
		}
		if !floadJudge {
			result.Result = false
			return &result
		}
	}

	////////////指数部分の判定//////////
	if startsWith(next, e) || startsWith(next, E) {
		ret = append(ret, E...)
		next = next[len(e):]
		var baseJudge = false
		if startsWith(next, plus) {
			ret = append(ret, plus...)
			next = next[len(plus):]
		} else if startsWith(next, minus) {
			ret = append(ret, minus...)
			next = next[len(minus):]
		}
		for len(next) > 0 {
			var findFlg = false
			for i := range numbers {
				if startsWith(next, numbers[i]) {
					if !baseJudge {
						baseJudge = true
					}
					findFlg = true
					ret = append(ret, numbers[i]...)
					next = next[len(numbers[i]):]
				}
			}
			if !findFlg {
				break
			}
		}

		if !baseJudge {
			result.Result = false
			return &result
		}
	}

	////////////パース成功//////////
	result.Result = true
	result.Next = next
	var value = JValue{}
	value.Vtype = JNumber
	value.Text = ret
	result.Value = &value
	return &result
}

func (parser JArrayParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	_, next := spSeq(input)
	var left = []byte("[")
	var right = []byte("]")
	var piriod = []byte(",")

	var value = JValue{}
	value.Vtype = JArray
	var values = []JValue{}
	result.Value = &value

	if startsWith(next, left) {
		next = next[len(left):]
	} else {
		result.Result = false
		return &result
	}

	var vResult = jValueParser(next)
	if vResult.Result {
		next = vResult.Next
		values = append(values, *vResult.Value)
		for len(next) > 0 {
			_, next = spSeq(next)
			if startsWith(next, piriod) {
				next = next[len(piriod):]
				vsValue := jValueParser(next)
				if vsValue.Result {
					next = vsValue.Next
					values = append(values, *vsValue.Value)
				} else {
					result.Result = false
					return &result
				}
			} else {
				break
			}
		}
	}
	_, next = spSeq(next)
	if startsWith(next, right) {
		next = next[len(right):]
	} else {
		result.Result = false
		return &result
	}
	_, next = spSeq(next)
	value.Nest = values
	result.Next = next
	result.Result = true
	return &result
}

func (parser JObjectParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	_, next := spSeq(input)
	var left = []byte("{")
	var right = []byte("}")
	var comma = []byte(",")

	var value = JValue{}
	value.Vtype = JObject
	var values = []JValue{}

	if startsWith(next, left) {
		next = next[len(left):]
	} else {
		result.Result = false
		return &result
	}
	_, next = spSeq(next)
	pResult, k, v, nexts := objectKeyValue(next)
	if pResult {
		values = append(values, *k.Value, *v.Value)
		_, next = spSeq(nexts)
		for len(next) > 0 {
			if startsWith(next, comma) {
				next = next[len(comma):]
				pResult, k, v, nexts = objectKeyValue(next)
				if pResult {
					_, next = spSeq(nexts)
					values = append(values, *k.Value, *v.Value)
				} else {
					result.Result = false
					return &result
				}
			} else {
				break
			}
		}
	}

	_, next = spSeq(next)
	if startsWith(next, right) {
		next = next[len(right):]
	} else {
		result.Result = false
		return &result
	}
	_, next = spSeq(next)
	result.Next = next
	result.Result = true
	value.Nest = values
	result.Value = &value
	return &result
}

func (parser JsonParser) Parse(input []byte) *JParseResult {
	var result = JParseResult{}
	result.Result = false
	parsers := []JParser{JObjectParser{}, JArrayParser{}}
	for i := range parsers {
		parseResult := parsers[i].Parse(input)
		if parseResult.Result {
			_, next := spSeq(parseResult.Next)
			if len(next) == 0 {
				result.Result = true
				result.Next = next
				result.Value = parseResult.Value
				break
			}
		}
	}
	return &result
}

func jValueParser(input []byte) *JParseResult {
	var parsers = []JParser{JNullParser{}, JBoolParser{}, JStringParser{}, JNumberParser{}, JArrayParser{}, JObjectParser{}}

	for i := range parsers {
		var parseResult = parsers[i].Parse(input)
		if parseResult.Result {
			return parseResult
		}
	}
	var fail = JParseResult{}
	fail.Result = false
	return &fail
}

func objectKeyValue(input []byte) (bool, *JParseResult, *JParseResult, []byte) {
	_, next := spSeq(input)
	var colon = []byte(":")
	var stringParser = JStringParser{}

	keyResult := stringParser.Parse(next)
	if keyResult.Result {
		_, next = spSeq(keyResult.Next)
		if startsWith(next, colon) {
			next = next[len(colon):]
		} else {
			return false, &JParseResult{}, &JParseResult{}, input
		}
		valueResult := jValueParser(next)

		if valueResult.Result {
			_, next = spSeq(valueResult.Next)
			return true, keyResult, valueResult, next
		} else {
			return false, &JParseResult{}, &JParseResult{}, input
		}
	} else {
		return false, &JParseResult{}, &JParseResult{}, input
	}
}

func scraps(input []byte, words [][]byte) (bool, []byte, []byte) {
	for i := range words {
		var result, text, next = scrap(input, words[i])
		if result {
			return result, text, next
		}
	}
	return false, []byte(""), input
}

func scrap(input []byte, word []byte) (bool, []byte, []byte) {
	if startsWith(input, word) {
		return true, word, input[len(word):]
	} else {
		return false, []byte(""), input
	}
}

func startsWith(input []byte, word []byte) bool {
	if len(input) < len(word) {
		return false
	}
	for i := range word {
		if input[i] != word[i] {
			return false
		}
	}
	return true
}

func wordSeq(input []byte, word []byte) ([]byte, []byte) {
	i := 0
	next := input
	for {
		if startsWith(next, word) && len(next) > 0 {
			i += len(word)
			next = next[len(word):]
		} else {
			return input[:i], next
		}
	}
}

func spSeq(input []byte) ([]byte, []byte) {
	return wordSeq(input, []byte(" "))
}
