package example.spray.json

trait JsonReader[T] {
  def read(json: JsValue): T
}

trait JsonFormat[T] extends JsonReader[T]