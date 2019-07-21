package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common._
import jp.co.teruuu.parser.json.`type`.{JArray, JValue}

object JArrayParser extends JParser[JArray] {
  lazy val sp = Parser.string(" ").many()
  lazy val leftP = new Pair3(sp, Parser.string("["), sp)
  lazy val rightP = new Pair3(sp, Parser.string("]"), sp)
  lazy val parser = (new Pair2(new Pair3(sp, JValueParser, sp), (new Pair2(Parser.string(","), (new Pair3(sp, JValueParser, sp)))).many())).option()


  override def parse(input: String): ParseResult[JArray] = {
    leftP.parse(input) match {
      case leftResult: ParseOk[(List[String], String, List[String])] => {
        parser.parse(leftResult.next) match {
          case result: ParseOk[Option[((List[String], JValue, List[String]), List[(String, (List[String], JValue, List[String]))])]] => {
            rightP.parse(result.next) match {
              case rightResult: ParseOk[(List[String], String, List[String])] => {
                var value = List.empty[JValue]
                if(result.value.isDefined) {
                  value = value :+ result.value.get._1._2
                  result.value.get._2.foreach(a => {
                    value = value :+ a._2._2
                  })
                }
                ParseOk(JArray(value), rightResult.next)
              }
              case rightResult: ParseNg[_] => ParseNg(rightResult.message, rightResult.next)
            }
          }
          case result: ParseNg[_] => ParseNg(result.message, result.next)
        }
      }
      case leftResult: ParseNg[_] => ParseNg(leftResult.message, leftResult.next)
    }
  }
}