package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk, ParseResult}

class JOrParser[T <: JValue,U <: JValue](parser1: JParser[T], parser2: JParser[U]) extends JParser[JValue] {

  override def parse(input: String): ParseResult[JValue] = {
    parser1.parse(input) match {
      case result1: ParseOk[T] => ParseOk(result1.value, result1.next)
      case result1: ParseNg[_] => {
        parser2.parse(input) match {
          case result2: ParseOk[U] => ParseOk(result2.value, result2.next)
          case result2: ParseNg[_] => ParseNg(result2.message, input)
        }
      }
    }
  }
}
