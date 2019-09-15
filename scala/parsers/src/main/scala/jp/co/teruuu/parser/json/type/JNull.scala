package jp.co.teruuu.parser.json.`type`

case class JNull() extends JValue {

  override def equals(obj: Any): Boolean = obj.isInstanceOf[JNull]

  def toSpray(): spray.json.JsValue = {
    spray.json.JsNull
  }
}
