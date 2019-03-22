package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{EOFParser, Parser}
import jp.co.teruuu.parser.json.`type`.{JNumber, JString, JValue}

trait JParser[T <: JValue] extends Parser[T] {
  def string():JParser[JString] = new JStringParser()
  def number(): JParser[JNumber] = new JNumberParser()
}

object JParser extends JParser[JValue]