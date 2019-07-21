package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common._
import jp.co.teruuu.parser.json.`type`.{JNumber, JString}

object JNumberParser extends JParser[JNumber] {
  lazy val signP = Parser.string("+").or(Parser.string("-")).option()
  lazy val baseP = Parser.string("E").or(Parser.string("e"))
  lazy val ztonP = Parser.string("0").or(Parser.string("1")).or(Parser.string("2")).or(Parser.string("3")).
    or(Parser.string("4")).or(Parser.string("5")).or(Parser.string("6")).
    or(Parser.string("7")).or(Parser.string("8")).or(Parser.string("9"))
  lazy val parser1 = new Pair2(new Pair2(signP, ztonP.plus()), (new Pair2(Parser.string("."), ztonP.plus())).option())
  lazy val parser2 = (new Pair3(baseP, signP, ztonP.plus())).option()

  override def parse(input: String): ParseResult[JNumber] = {
    parser1.parse(input) match {
      case result1: ParseOk[((Option[String], List[String]), Option[(String, List[String])])] => {
        parser2.parse(result1.next) match {
          case result2: ParseOk[Option[(String, Option[String], List[String])]] => {
            var value = 0
            var deci = 0
            var base = 0
            val valueStr = result1.value._1._2.foldLeft(result1.value._1._1.getOrElse(""))((acc, cur) => {
              acc + cur
            })
            value = Integer.valueOf(valueStr)
            if(result1.value._2.isDefined) {
              val decStr = result1.value._2.get._2.foldLeft("")((acc, cur) => {
                acc + cur
              })
              deci = Integer.valueOf(decStr)
            }
            if(result2.value.isDefined) {
              val baseStr = result2.value.get._3.foldLeft(result2.value.get._2.getOrElse(""))((acc, cur) => {
                acc + cur
              })
              base = Integer.valueOf(baseStr)
            }
            ParseOk(JNumber(value, deci, base), result2.next)
          }
          case result2: ParseNg[_] => ParseNg(result2.message, result2.next)
        }
      }
      case result1: ParseNg[_] => ParseNg(result1.message, result1.next)
    }
  }
}
