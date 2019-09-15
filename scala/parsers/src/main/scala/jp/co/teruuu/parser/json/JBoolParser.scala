package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common.{ParseNg, ParseOk, ParseResult, Parser}

object JBoolParser extends JParser[JBool] {

  override def parse(input: String): ParseResult[JBool] = {
    val parser = Parser.string("true").or(Parser.string("false"))
    parser.parse(input) match {
      case result: ParseOk[String] => ParseOk(JBool(result.value.equals("true")), result.next)
      case result: ParseNg[_] => ParseNg(result.message, result.next)
    }
  }
}
