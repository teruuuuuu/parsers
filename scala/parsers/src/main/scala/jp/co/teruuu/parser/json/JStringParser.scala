package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common._

object JStringParser extends JParser[JString] {

  override def parse(input: String): ParseResult[JString] = {
    val parser = new Pair3(Parser.string("\""), Parser.stop("\"", Map("\\\"" -> "\"")), Parser.string("\""))
    parser.parse(input) match {
      case result: ParseOk[(String, String, String)] => ParseOk(JString(result.value._2), result.next)
      case result: ParseNg[_] => ParseNg(result.message, result.next)
    }
  }
}
