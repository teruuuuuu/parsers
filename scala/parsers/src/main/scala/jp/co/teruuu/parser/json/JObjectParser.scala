package jp.co.teruuu.parser.json

import jp.co.teruuu.parser.common._
import jp.co.teruuu.parser.json.`type`.{JArray, JObject, JString, JValue}

object JObjectParser extends JParser[JObject] {
  lazy val sp = Parser.string(" ").many()
  lazy val leftP = new Pair3(sp, Parser.string("{"), sp)
  lazy val rightP = new Pair3(sp, Parser.string("}"), sp)
  lazy val kvP = new Pair3(new Pair3(sp, JStringParser, sp), Parser.string(":"), new Pair3(sp, JValueParser, sp))
  lazy val kvsP = new Pair2(Parser.string(","), kvP)
  lazy val parser = (new Pair2(kvP, kvsP.many())).option()

  override def parse(input: String): ParseResult[JObject] = {
    leftP.parse(input) match {
      case leftResult: ParseOk[(List[String], String, List[String])] => {
        parser.parse(leftResult.next) match {
          case result: ParseOk[Option[(((List[String], JString, List[String]), String, (List[String], JValue, List[String])),
            List[((String, ((List[String], JString, List[String]), String, (List[String], JValue, List[String]))))])]] => {
            rightP.parse(result.next) match {
              case rightResult: ParseOk[(List[String], String, List[String])] => {
                var map: Map[JString, JValue] = Map()
                if(result.value.isDefined) {
                  map += result.value.get._1._1._2 -> result.value.get._1._3._2
                  result.value.get._2.foreach(a => {
                    map += a._2._1._2 -> a._2._3._2
                  })
                }
                ParseOk(JObject(map), rightResult.next)
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
