package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class OptionalParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = Parser.string("Hello").option()
    val result1 = parser.parse("Hello, World")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[Option[String]]].value.equals(Some("Hello")))

    val result2 = parser.parse("Hell, World")
    assert(result2.isInstanceOf[ParseOk[_]])
    assert(result2.asInstanceOf[ParseOk[Option[String]]].value.equals(None))
  }

}
