package jp.co.teruuu.parser.json.`type`

trait JValue {
  def toSpray(): spray.json.JsValue

//  import fommil.sjs.FamilyFormats._
//  def convertTo[A] = toSpray.convertTo[A]
}
