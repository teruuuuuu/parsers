package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class ManyParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = Parser.string("Hello").many()
    val result1 = parser.parse("HelloHello")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[List[String]]].value.equals(List("Hello", "Hello")))

    val result2 = parser.parse("")
    assert(result2.isInstanceOf[ParseOk[_]])
    assert(result2.asInstanceOf[ParseOk[List[String]]].value.equals(List()))
  }

}
