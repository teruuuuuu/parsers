package jp.co.teruuu.parser.common

class OptionalParser[T](parser: Parser[T]) extends Parser[Option[T]] {

  override def parse(input: String): ParseResult[Option[T]] = {
    parser.parse(input) match {
      case result: ParseOk[T] => ParseOk(Some(result.value), result.next)
      case result: ParseNg[_] => ParseOk(None, input)
    }
  }
}
