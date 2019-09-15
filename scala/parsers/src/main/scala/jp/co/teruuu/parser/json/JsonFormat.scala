package jp.co.teruuu.parser.json

trait JsonReader[T] {
  def read(json: JValue): T
}

object JsonReader {
  implicit def func2Reader[T](f: JValue => T): JsonReader[T] = new JsonReader[T] {
    def read(json: JValue) = f(json)
  }
}

trait JsonFormat[T] extends JsonReader[T]