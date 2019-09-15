package example.spray

package object json {
  def serializationError(msg: String) = throw new SerializationException(msg)
  def jsonReader[T](implicit reader: JsonReader[T]) = reader
}

package json {
  class SerializationException(msg: String) extends RuntimeException(msg)
}