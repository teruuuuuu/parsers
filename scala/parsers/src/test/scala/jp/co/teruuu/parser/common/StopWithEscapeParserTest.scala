package jp.co.teruuu.parser.common

import org.scalatest.FlatSpec;

class StopWithEscapeParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser1 = Parser.stop("\"", Map("\\\"" -> "\""))

    val result1 = parser1.parse("Hello\", world")
    assert(result1.isInstanceOf[ParseOk[String]])
    assert(result1.asInstanceOf[ParseOk[String]].value.equals("Hello"))

    val parser2 = new Pair3(Parser.string("\""), parser1, Parser.string("\""))
    val result2 = parser2.parse("\"Hello\", world\"")
    assert(result2.isInstanceOf[ParseOk[(String, String, String)]])
    assert(result2.asInstanceOf[ParseOk[(String, String, String)]].value._2.equals("Hello"))

    val result3 = parser2.parse("\"Hello\\\", world\"")
    assert(result3.isInstanceOf[ParseOk[(String, String, String)]])
    assert(result3.asInstanceOf[ParseOk[(String, String, String)]].value._2.equals("Hello\", world"))
  }

}
