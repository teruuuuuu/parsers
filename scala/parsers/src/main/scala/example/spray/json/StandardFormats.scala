package example.spray.json

trait StandardFormats {

  private[json] type JF[T] = JsonFormat[T] // simple alias for reduced verbosity

  implicit def optionFormat[T :JF]: JF[Option[T]] = new OptionFormat[T]

  class OptionFormat[T :JF] extends JF[Option[T]] {
    def read(value: JsValue) = value match {
      case JsNull => None
      case x => Some(x.convertTo[T])
    }
    // allows reading the JSON as a Some (useful in container formats)
    def readSome(value: JsValue) = Some(value.convertTo[T])
  }
}
