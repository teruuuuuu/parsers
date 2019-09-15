package jp.co.teruuu.parser.json.`type`

case class JObject(map: Map[JString, JValue]) extends JValue {
  def getMap = map

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: JObject => {
        (map.size == obj.getMap.size) && map.find(kv => !kv._2.equals(obj.getMap.get(kv._1).get)).isEmpty
      }
      case _ => false
    }
  }

  def toSpray(): spray.json.JsValue = {
    spray.json.JsObject(map.toList.map{a => a match{
      case (key, value) => (key.value, value.toSpray())
    }}.toMap)
  }
}
