package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{OrParser, Parser}

trait JParser[T <: JValue] extends Parser[T] {
  def jor[T <: JValue](parser: JParser[T]): JParser[JValue] = new JOrParser(this, parser)
}

object JParser extends JParser[JValue]