package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class OrParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = Parser.string("Hello").or(Parser.string("World"))

    val result1 = parser.parse("HelloWorld")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[String]].value.equals("Hello"))

    val result2 = parser.parse("WorldHello")
    assert(result2.isInstanceOf[ParseOk[_]])
    assert(result2.asInstanceOf[ParseOk[String]].value.equals("World"))

    val result3 = parser.parse("helloWorld")
    assert(result3.isInstanceOf[ParseNg[_]])
  }

}
