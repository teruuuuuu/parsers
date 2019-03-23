package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk, ParseResult, Parser}
import jp.co.teruuu.parser.json.`type`.JNull

object JNullParser extends JParser[JNull] {

  override def parse(input: String): ParseResult[JNull] = {
    val parser = Parser.string("null")
    parser.parse(input) match {
      case result: ParseOk[String] => ParseOk(JNull(), result.next)
      case result: ParseNg[_] => ParseNg(result.message, result.next)
    }
  }
}
