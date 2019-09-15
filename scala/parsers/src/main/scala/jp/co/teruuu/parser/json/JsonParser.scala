package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common._

object JsonParser extends JParser[JValue]{
  lazy val parser = new Pair2(JObjectParser.jor(JArrayParser), Parser.eof)

  override def parse(input: String): ParseResult[JValue] = {
    parser.parse(input) match {
      case result: ParseOk[(JValue, String)] => ParseOk(result.value._1, result.next)
      case result: ParseNg[_] => ParseNg(result.message, result.next)
    }
  }
}
