package parser

import (
	"fmt"
	"parser/common"
	"parser/json"
	"testing"
)

func Test(t *testing.T) {
	var p = json.StringParser{}
	fmt.Println(p.Parse("aa"))

	var a = common.StringParser{}
	fmt.Println(a)

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
