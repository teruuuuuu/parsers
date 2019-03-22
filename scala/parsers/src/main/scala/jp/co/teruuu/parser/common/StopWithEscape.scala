package jp.co.teruuu.parser.common

class StopWithEscape(stop: String, escapes: Map[String, String]) extends Parser[String] {
  assert(stop.length > 0)

  override def parse(input: String): ParseResult[String] = {
    var next = input
    var continueFlg = true
    var ret = ""
    while(continueFlg) {
      escapes.foreach{case (key, value) => {
        if(next.startsWith(key)) {
          ret += value
          next = next.substring(key.length)
        }
      }}
      if(next.length > 0 && !next.startsWith(stop)) {
        ret += next(0)
        next = next.substring(1)
      } else {
        continueFlg = false
      }
    }
    ParseOk(ret, next)
  }
}
