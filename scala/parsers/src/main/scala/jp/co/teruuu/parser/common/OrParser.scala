package jp.co.teruuu.parser.common

class OrParser[T](lps: Parser[T], rps: Parser[T]) extends Parser[T] {

  override def parse(input: String): ParseResult[T] = {
    lps.parse(input) match {
      case lResult: ParseOk[T] => ParseOk(lResult.value, lResult.next)
      case lResult: ParseNg[_] => {
        rps.parse(input) match {
          case rResult: ParseOk[T] => ParseOk(rResult.value, rResult.next)
          case rResult: ParseNg[_] => ParseNg(rResult.message, rResult.next)
        }
      }
    }
  }
}
