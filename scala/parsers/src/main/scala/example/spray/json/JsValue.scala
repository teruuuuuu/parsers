package example.spray.json

sealed abstract class JsValue {
  def convertTo[T :JsonReader]: T = jsonReader[T].read(this)
}

case class JsString(value: String) extends JsValue
object JsString {
  val empty = JsString("")
  def apply(value: Symbol) = new JsString(value.name)
}

case object JsNull extends JsValue