package example

import example.spray.json.JsNull
import jp.co.teruuu.parser.json.{DefaultJsonProtocol, JString}

object MyJsonConvert extends App {

  object a extends DefaultJsonProtocol {
    def test() = {
      println(JString("Hello").convertTo[Option[String]])
    }
  }
  a.test
}
