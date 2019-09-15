package example


object TrySprayShapeless extends App {

  import spray.json._
  import fommil.sjs.FamilyFormats._

  case class Foo(a: String, b: Double, c: Boolean)
  case class Bar(d: String, e: Double, f: Boolean)

  val sprayJsonConvertResult = """{"a":"foo","b":42.0,"c":true}""".parseJson.convertTo[Foo]
  println(sprayJsonConvertResult)

  import jp.co.teruuu.parser.json.{JsonParser => MyParser}
  import jp.co.teruuu.parser.json.`type`.{JValue => MyJValue}
  import jp.co.teruuu.parser.common.{ParseOk => MyParseOk}
  val myParseResult = MyParser.parse(" {\"a\": \"foo\", \"b\": 42.0, \"c\": true}")
  val myParseResultValue = myParseResult.asInstanceOf[MyParseOk[MyJValue]].value
  val myParserToSpray = myParseResultValue.toSpray()
  val myParserToSprayToConvert = myParserToSpray.convertTo[Foo]
  println(myParserToSprayToConvert)

  assert(sprayJsonConvertResult equals myParserToSprayToConvert)

}
