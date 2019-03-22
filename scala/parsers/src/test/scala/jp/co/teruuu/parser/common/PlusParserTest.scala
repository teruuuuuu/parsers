package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class PlusParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = Parser.string("Hello").plus()
    val result1 = parser.parse("HelloHello")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[List[String]]].value.equals(List("Hello", "Hello")))

    val result2 = parser.parse("hello")
    assert(result2.isInstanceOf[ParseNg[_]])
  }

}
