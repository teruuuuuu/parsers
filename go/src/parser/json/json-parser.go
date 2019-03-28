package json

import "fmt"

const (
	JNull int8 = iota
	JBool
	JString
	JNumber
	JArray
	JObject
)

type JValue struct {
	vtype int8
	text  string
	nest  []JValue
}

type ParseResult struct {
	result  bool
	next    string
	value   JValue
	message string
}

type Parser interface {
	Parse(input string) *ParseResult
}

type StringParser struct{}

func (parser *StringParser) Parse(input string) *ParseResult {
	var inpu = []byte("Hello, World")
	var word = []byte("Hello")
	fmt.Println(startsWith(inpu, word))

	return &ParseResult{}
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
