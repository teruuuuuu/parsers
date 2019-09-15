package jp.co.teruuu.parser.json

trait JValue {
  def toSpray(): spray.json.JsValue

  def convertTo[T :JsonReader]: T = jsonReader[T].read(this)
//  import fommil.sjs.FamilyFormats._
//  def convertTo[A] = toSpray.convertTo[A]
}
