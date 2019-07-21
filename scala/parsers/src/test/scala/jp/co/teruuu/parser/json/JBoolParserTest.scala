package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk}
import jp.co.teruuu.parser.json.`type`.JBool
import org.scalatest.FlatSpec

class JBoolParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JBoolParser

    val result1 = parser.parse("true")
    assert(result1.isInstanceOf[ParseOk[JBool]])
    assert(result1.asInstanceOf[ParseOk[JBool]].value.equals(JBool(true)))

    val result2 = parser.parse("false")
    assert(result2.isInstanceOf[ParseOk[JBool]])
    assert(result2.asInstanceOf[ParseOk[JBool]].value.equals(JBool(false)))
  }

  "parse" should "fail" in {
    val parser = JBoolParser

    val result1 = parser.parse("True")
    assert(result1.isInstanceOf[ParseNg[JBool]])
  }
}
