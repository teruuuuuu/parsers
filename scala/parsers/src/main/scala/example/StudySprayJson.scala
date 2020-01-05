package example

import example.spray.json.{DefaultJsonProtocol, JsNull, JsString}

object StudySprayJson extends App {

  object a extends DefaultJsonProtocol {
    def test() = {
      println(JsString("Hello").convertTo[Option[String]] equals Some("Hello"))
    }
  }
  a.test
}
