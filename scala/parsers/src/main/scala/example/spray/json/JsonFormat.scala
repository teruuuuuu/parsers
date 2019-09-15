package example.spray.json

trait JsonReader[T] {
  def read(json: JsValue): T
}

object JsonReader {
  implicit def func2Reader[T](f: JsValue => T): JsonReader[T] = new JsonReader[T] {
    def read(json: JsValue) = f(json)
  }
}

trait JsonFormat[T] extends JsonReader[T]