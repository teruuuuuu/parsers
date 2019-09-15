package jp.co.teruuu.parser.json.`type`

case class JArray(value: List[JValue]) extends JValue {
  def getVal = value

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: JArray => value.length == obj.getVal.length &&
        value.zipWithIndex.find{case (v, index) => !obj.getVal(index).equals(v)}.isEmpty
      case _ => false
    }
  }

  def toSpray(): spray.json.JsValue = {
    spray.json.JsArray(value.map(_.toSpray()).toVector)
  }

}
