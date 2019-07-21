package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.ParseResult
import jp.co.teruuu.parser.json.`type`.JValue

object JValueParser extends JParser[JValue] {
  lazy val parser = JNullParser.jor(JBoolParser).jor(JStringParser).jor(JNumberParser).jor(JArrayParser).jor(JObjectParser)
  override def parse(input: String): ParseResult[JValue] = parser.parse(input)
}
