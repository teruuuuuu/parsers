package jp.co.teruuu.parser.json

case class JNull() extends JValue {

  override def equals(obj: Any): Boolean = obj.isInstanceOf[JNull]

  def toSpray(): spray.json.JsValue = {
    spray.json.JsNull
  }
}
