package jp.co.teruuu.parser.common

class EOFParser() extends Parser[String] {

  override def parse(input: String): ParseResult[String] = {
    input.length == 0 match {
      case true => ParseOk("", "")
      case false => ParseNg("expect EOF:, actual " + input, input)
    }
  }
}
