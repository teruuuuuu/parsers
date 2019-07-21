package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.ParseOk
import jp.co.teruuu.parser.json.`type`._
import org.scalatest.FlatSpec

class JObjectParserTest extends FlatSpec{

  "parse" should "successt" in {
    val parser = JObjectParser

    val result1 = parser.parse(" { } ")
    assert(result1.isInstanceOf[ParseOk[JObject]])
    assert(result1.asInstanceOf[ParseOk[JObject]].value.equals(JObject(Map.empty[JString, JValue])))

    val result2 = parser.parse("{\"a\": 1, \"c\": [{\"d\": 345}]}")
    assert(result2.isInstanceOf[ParseOk[JObject]])
    assert(result2.asInstanceOf[ParseOk[JObject]].value.equals(JObject(
      Map(JString("a") -> JNumber(1, 0, 0), JString("c") -> JArray(List(JObject(Map(JString("d") -> JNumber(345, 0, 0))))))
    )))

  }

}
