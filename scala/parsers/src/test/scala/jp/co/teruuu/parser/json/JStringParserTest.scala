package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk}
import jp.co.teruuu.parser.json.`type`.JString
import org.scalatest.FlatSpec

class JStringParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JParser.string()

    val result1 = parser.parse("\"key\"")
    assert(result1.isInstanceOf[ParseOk[_]])

    val result2 = parser.parse("\"esca\\\"pe\"")
    assert(result2.isInstanceOf[ParseOk[JString]])
    assert(result2.asInstanceOf[ParseOk[JString]].value.equals(JString("esca\"pe")))

    val result3 = parser.parse("key")
    assert(result3.isInstanceOf[ParseNg[_]])
  }
}
