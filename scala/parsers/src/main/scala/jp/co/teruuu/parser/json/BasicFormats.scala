package jp.co.teruuu.parser.json

trait BasicFormats {

  implicit object StringJsonFormat extends JsonFormat[String] {
    def read(value: JValue) = value match {
      case JString(x) => x
      //      case x => deserializationError("Expected String as JsString, but got " + x)
    }
  }
}
