package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.ParseOk
import jp.co.teruuu.parser.json.`type`._
import org.scalatest.FlatSpec

class JArrayParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JParser.array()

    val result1 = parser.parse(" [ ] ")
    assert(result1.isInstanceOf[ParseOk[JArray]])
    assert(result1.asInstanceOf[ParseOk[JArray]].value.equals(JArray(List.empty)))

    val result2 = parser.parse(" [ 1 ] ")
    val ans2 = JArray(List(JNumber(1, 0, 0)))
    assert(result2.isInstanceOf[ParseOk[JArray]])
    assert(result2.asInstanceOf[ParseOk[JArray]].value.equals(ans2))

    val result3 = parser.parse(" [ 1 , \"2\" ] ")
    val ans3 = JArray(List(JNumber(1, 0, 0), JString("2")))
    assert(result3.isInstanceOf[ParseOk[JArray]])
    assert(result3.asInstanceOf[ParseOk[JArray]].value.equals(ans3))

    val result4 = parser.parse(" [ 1 , \"2\" , [ true, null, [] ]]")
    val ans4 = JArray(List(JNumber(1, 0, 0), JString("2"), JArray(List(JBool(true), JNull(), JArray(List())))))
    assert(result4.isInstanceOf[ParseOk[JArray]])
    assert(result4.asInstanceOf[ParseOk[JArray]].value.equals(ans4))

  }

}
