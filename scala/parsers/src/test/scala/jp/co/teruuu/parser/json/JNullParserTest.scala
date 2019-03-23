package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk}
import jp.co.teruuu.parser.json.`type`.{JBool, JNull}
import org.scalatest.FlatSpec

class JNullParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JParser.nu()

    val result1 = parser.parse("null")
    assert(result1.isInstanceOf[ParseOk[JNull]])
    assert(result1.asInstanceOf[ParseOk[JNull]].value.equals(JNull()))
  }

  "parse" should "fail" in {
    val parser = JParser.nu()

    val result1 = parser.parse("NULL")
    assert(result1.isInstanceOf[ParseNg[JNull]])
  }
}
