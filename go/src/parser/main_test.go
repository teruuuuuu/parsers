package parser

import (
	"fmt"
	"parser/json"
	"testing"
	"unsafe"
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
	fmt.Println(*(*string)(unsafe.Pointer(&result1.Next)))
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

func TestJArray(t *testing.T) {
	var parser = json.JArrayParser{}
	var result1 = parser.Parse([]byte(" [   ]  "))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}

	var result2 = parser.Parse([]byte(" [  \"abc\" ]  "))
	result2.Show()
	if !result2.Result {
		t.Errorf("parse failed")
	}

	var result3 = parser.Parse([]byte(" [  {}, {} ]  "))
	result3.Show()
	if !result3.Result {
		t.Errorf("parse failed")
	}
}

func TestJObject(t *testing.T) {
	var parser = json.JObjectParser{}
	var result1 = parser.Parse([]byte(" { \"abc\" : 123  }"))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}

	var result2 = parser.Parse([]byte(" {   }"))
	result2.Show()
	if !result2.Result {
		t.Errorf("parse failed")
	}

	var result3 = parser.Parse([]byte(" { \"def\": [ ]  }"))
	result3.Show()
	if !result3.Result {
		t.Errorf("parse failed")
	}

	var result4 = parser.Parse([]byte(" { \"ghi\": [ { }, { \"jkl\": 345 }]  }"))
	result4.Show()
	if !result4.Result {
		t.Errorf("parse failed")
	}
	format4 := result4.Value.Format()
	var view4 = ""
	for i := range format4 {
		view4 += format4[i] + "\n"
	}
	fmt.Println(view4)
}

func TestJson(t *testing.T) {
	var parser = json.JsonParser{}
	var result1 = parser.Parse([]byte(" { \"abc\" : 123  }"))
	result1.Show()
	if !result1.Result {
		t.Errorf("parse failed")
	}

	var result2 = parser.Parse([]byte(" {   }"))
	result2.Show()
	if !result2.Result {
		t.Errorf("parse failed")
	}

	var result3 = parser.Parse([]byte(" { \"def\": [ ]  }"))
	result3.Show()
	if !result3.Result {
		t.Errorf("parse failed")
	}

	var result4 = parser.Parse([]byte(" { \"ghi\": [ { }, { \"jkl\": 345 }]  }"))
	result4.Show()
	if !result4.Result {
		t.Errorf("parse failed")
	}

	var result5 = parser.Parse([]byte(" [  \"abc\" ]  "))
	result5.Show()
	if !result5.Result {
		t.Errorf("parse failed")
	}

	var result6 = parser.Parse([]byte(" [  \"abc\" ], {}"))
	result6.Show()
	if result6.Result {
		t.Errorf("parse failed")
	}
}
