package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class Pair3ParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = new Pair3(Parser.string("Hello"), Parser.string(","), Parser.string("World"))
    val result1 = parser.pair2(Parser.eof).parse("Hello,World")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[((String, String, String), String)]].value._1.equals(("Hello", ",", "World")))

    val result2 = parser.parse("Hello,Wor")
    assert(result2.isInstanceOf[ParseNg[_]])

    val result3 = parser.parse("HelloWorld")
    assert(result3.isInstanceOf[ParseNg[_]])

    val result4 = parser.parse("Hello")
    assert(result4.isInstanceOf[ParseNg[_]])
  }

}
