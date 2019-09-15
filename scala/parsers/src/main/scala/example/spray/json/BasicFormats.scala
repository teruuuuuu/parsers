package example.spray.json

trait BasicFormats {

  implicit object StringJsonFormat extends JsonFormat[String] {
    def read(value: JsValue) = value match {
      case JsString(x) => x
//      case x => deserializationError("Expected String as JsString, but got " + x)
    }
  }
}
