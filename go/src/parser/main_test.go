package parser

import (
	"parser/json"
	"testing"
)

func TestJNull(t *testing.T) {
	var parser = json.JNullParser{}
	var result1 = parser.Parse([]byte("   null  "))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}
}

func TestJBool(t *testing.T) {
	var parser = json.JBoolParser{}
	var result1 = parser.Parse([]byte("   true  "))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}

	var result2 = parser.Parse([]byte("   false"))
	result2.Show()
	if !result2.Result {
		t.Errorf("parse failed")
	}

	var result3 = parser.Parse([]byte("   True"))
	result3.Show()
	if result3.Result {
		t.Errorf("parse failed")
	}
}

func TestJString(t *testing.T) {
	var parser = json.JStringParser{}
	var result1 = parser.Parse([]byte("   \"Hello\", World\"  "))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}

	var result2 = parser.Parse([]byte("   \"Hello, World  "))
	result2.Show()
	if result2.Result {
		t.Errorf("parse failed")
	}

	var result3 = parser.Parse([]byte("   \"Hello\\\", World\"  "))
	result3.Show()
	if !result3.Result {
		t.Errorf("parse failed")
	}
}

func TestJNumber(t *testing.T) {
	var parser = json.JNumberParser{}
	var result1 = parser.Parse([]byte("   +123.456E-789  "))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}
}

func Test(t *testing.T) {

	var result int

	// テストケースの検証.
	// テストしたい関数の実行結果を if などで判定して想定した値であるかを検証する.
	result = 3
	if result != 3 {
		// テスト失敗時には t.Error などでエラーを表示する.
		t.Errorf("add failed. expect:%d, actual:%d", 3, result)
	}

	// テスト中のロギング.
	// t.Log, t.Logf でログを出すと `go test -v` と実行したときのみ表示される.
	t.Logf("result is %d", result)
}
