package jp.co.teruuu.parser.json.`type`

case class JBool(value: Boolean) extends JValue {
  override def equals(obj: Any): Boolean = obj.isInstanceOf[JBool] && value.equals(obj.asInstanceOf[JBool].value)
}
