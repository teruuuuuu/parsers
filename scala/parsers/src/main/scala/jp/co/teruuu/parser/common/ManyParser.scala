package jp.co.teruuu.parser.common

class ManyParser[T](parser: Parser[T]) extends Parser[List[T]] {

  override def parse(input: String): ParseResult[List[T]] = {
    var next = input
    var values = List.empty[T]
    var isContinue = true
    while(isContinue) {
      val result = parser.parse(next)
      if(result.isInstanceOf[ParseOk[_]]) {
        values = values :+ result.asInstanceOf[ParseOk[T]].value
        next = result.asInstanceOf[ParseOk[T]].next
      } else {
        isContinue = false
      }
    }
    ParseOk(values, next)
  }
}
