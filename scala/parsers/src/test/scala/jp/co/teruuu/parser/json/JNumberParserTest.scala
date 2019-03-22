package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.ParseOk
import jp.co.teruuu.parser.json.`type`.{JNumber, JString}
import org.scalatest.FlatSpec

class JNumberParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JParser.number()

    val result1 = parser.parse("123.456e-234")
    assert(result1.isInstanceOf[ParseOk[_]])
    assert(result1.asInstanceOf[ParseOk[JNumber]].value.equals(JNumber(123,456, -234)))

    val result2 = parser.parse("0.5")
    assert(result2.isInstanceOf[ParseOk[_]])
    assert(result2.asInstanceOf[ParseOk[JNumber]].value.equals(JNumber(0,5, 0)))

  }
}
