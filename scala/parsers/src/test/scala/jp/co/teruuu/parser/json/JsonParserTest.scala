package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk}
import jp.co.teruuu.parser.json.`type`._
import org.scalatest.FlatSpec

class JsonParserTest extends FlatSpec {

  "parse" should "successt" in {
    val parser = JParser.json

    val result1 = parser.parse(" {\"a\": [ [1, 2, 3,4], {} ]} ")
    assert(result1.isInstanceOf[ParseOk[JValue]])
    assert(result1.asInstanceOf[ParseOk[JValue]].value.equals(JObject(
      Map(JString("a") -> JArray(List(JArray(List(JNumber(1, 0, 0), JNumber(2, 0, 0), JNumber(3, 0, 0), JNumber(4, 0, 0))), JObject(Map.empty[JString, JValue]))))
    )))


    val result2 = parser.parse(" [ [1, 2, 3,4], {} ] ")
    assert(result2.isInstanceOf[ParseOk[JValue]])
    assert(result2.asInstanceOf[ParseOk[JValue]].value.equals(JArray(
      List(JArray(List(JNumber(1, 0, 0), JNumber(2, 0, 0), JNumber(3, 0, 0), JNumber(4, 0, 0))), JObject(Map.empty[JString, JValue]))
    )))
  }

  "parse" should "fail" in {
    val parser = JParser.json

    val result1 = parser.parse(" {\"a\": [ [1, 2, 3,4], {} ]}, {}")
    assert(result1.isInstanceOf[ParseNg[_]])
  }
}
