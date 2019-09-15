package example

import example.spray.json.{DefaultJsonProtocol, JsString}

object StudySprayJson extends App {

  object a extends DefaultJsonProtocol {
    def test() = {
      println(JsString("Hello").convertTo[Option[String]])
    }
  }
  a.test

}
