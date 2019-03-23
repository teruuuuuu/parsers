package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{OrParser, Parser}
import jp.co.teruuu.parser.json.`type`._

trait JParser[T <: JValue] extends Parser[T] {
  def json(): JParser[JValue] = JsonParser
  def nu(): JParser[JNull] = JNullParser
  def bool(): JParser[JBool] = JBoolParser
  def string():JParser[JString] = JStringParser
  def number(): JParser[JNumber] = JNumberParser
  def array(): JParser[JArray] = JArrayParser
  def obj(): JParser[JObject] = JObjectParser

  def jor[T <: JValue](parser: JParser[T]): JParser[JValue] = new JOrParser(this, parser)
  def value(): JParser[JValue] = JNullParser.jor(JBoolParser).jor(JStringParser).jor(JNumberParser).jor(JArrayParser).jor(JObjectParser)
}

object JParser extends JParser[JValue]