package jp.co.teruuu.parser.json.`type`

case class JString(value: String) extends JValue {

  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[JString] && value.equals(obj.asInstanceOf[JString].value)
  }

  def toSpray(): spray.json.JsValue = {
    spray.json.JsString(value)
  }
}
