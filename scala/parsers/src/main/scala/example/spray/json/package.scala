package example.spray

package object json {
  def deserializationError(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) = throw new DeserializationException(msg, cause, fieldNames)
  def jsonReader[T](implicit reader: JsonReader[T]) = reader
}

package json {
  case class DeserializationException(msg: String, cause: Throwable = null, fieldNames: List[String] = Nil) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)
}