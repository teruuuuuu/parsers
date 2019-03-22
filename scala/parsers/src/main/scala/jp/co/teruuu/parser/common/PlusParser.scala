package jp.co.teruuu.parser.common

class PlusParser[T](parser: Parser[T]) extends Parser[List[T]] {

  override def parse(input: String): ParseResult[List[T]] = {
    var next = input
    var values = List.empty[T]
    var isContinue = true
    var result: ParseResult[T] = null
    while(isContinue) {
      result = parser.parse(next)
      if(result.isInstanceOf[ParseOk[_]]) {
        values = values :+ result.asInstanceOf[ParseOk[T]].value
        next = result.asInstanceOf[ParseOk[T]].next
      } else {
        isContinue = false
      }
    }
    values.length match {
      case 0 => ParseNg(result.asInstanceOf[ParseNg[_]].message, input)
      case _ => ParseOk(values, next)
    }
  }
}
