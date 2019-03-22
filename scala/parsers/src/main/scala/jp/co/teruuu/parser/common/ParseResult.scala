package jp.co.teruuu.parser.common

trait ParseResult[T] {
}

case class ParseOk[T](value: T, next: String) extends ParseResult[T] {
  override def toString(): String = "Success(" + value + ", " + next + ")"
}

case class ParseNg[T](message: String, next: String) extends ParseResult[T] {
  override def toString(): String = "Failure(" + message + ", " + next + ")"
}
