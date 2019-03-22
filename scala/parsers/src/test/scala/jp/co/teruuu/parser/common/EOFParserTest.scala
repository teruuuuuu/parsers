package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class EOFParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = Parser.eof
    val result1 = parser.parse("")
    assert(result1.isInstanceOf[ParseOk[_]])
  }

  "parse" should "failer" in {
    val parser = Parser.eof
    val result1 = parser.parse("Hello")
    assert(result1.isInstanceOf[ParseNg[_]])
  }
}
