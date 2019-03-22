package jp.co.teruuu.parser.common

trait Parser[T] {
  def parse(input: String): ParseResult[T] = ???

  def many(): Parser[List[T]] = new ManyParser[T](this)
  def plus(): Parser[List[T]] = new PlusParser[T](this)
  def pair2[U](rps: Parser[U]): Parser[(T,U)] = new Pair2(this, rps)
  def pair3[U,V](cps: Parser[U], rps: Parser[V]): Parser[(T,U,V)] = new Pair3(this, cps, rps)
  def or(p: Parser[T]): Parser[T] = new OrParser(this, p)
  def option(): Parser[Option[T]] = new OptionalParser[T](this)
  def eof:Parser[String] = new EOFParser()
  def string(literal: String):Parser[String] = new StringParser(literal)
  def stop(stop: String, escapes: Map[String, String]):Parser[String] = new StopWithEscape(stop, escapes)

}

object Parser extends Parser[Any] {}