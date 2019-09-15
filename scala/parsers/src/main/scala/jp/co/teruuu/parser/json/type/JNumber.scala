package jp.co.teruuu.parser.json.`type`

case class JNumber(value: Int, decimal: Int, base: Int) extends JValue {

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: JNumber => value == obj.value && decimal == obj.decimal && base == obj.base
      case _ => false
    }
  }

  def toSpray(): spray.json.JsValue = {
    spray.json.JsNumber((value * Math.pow(10, base)))
  }
}
