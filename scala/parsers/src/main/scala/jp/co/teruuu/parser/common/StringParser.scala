package jp.co.teruuu.parser.common

class StringParser(literal: String) extends Parser[String] {

  override def parse(input: String): ParseResult[String] = {
    input.startsWith(literal) match {
      case true => ParseOk(this.literal, input.substring(literal.length))
      case false => ParseNg("expect: " + this.literal, input)
    }
  }
}
