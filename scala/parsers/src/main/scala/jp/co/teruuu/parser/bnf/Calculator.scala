package jp.co.teruuu.parser.bnf

import jp.co.teruuu.parser.bnf.parser._

object calculator {
  def E: P[Int] = rule(A)
  def A: P[Int] = rule {
    (M ~ ($("+") ~ M | $("-") ~ M).*).map {case x ~ ys =>
      ys.foldLeft(x){ case (l, op ~ r) => if(op == "+") l + r else l -r } }
  }
  def M: P[Int] = rule {
    (P ~ ($("*") ~ P | $("/") ~ P).*).map { case x ~ ys =>
      ys.foldLeft(x) { case (l, op ~ r) => if (op == "*") l * r else l / r } }
  }
  def P: P[Int] = rule {
    ($("(") ~ E ~ $(")")).map { case _ ~ e ~ _ => e } | L
  }
  def L: P[Int] = rule {
    (N+) map{_.foldLeft(0)((acc, cur) => acc*10+cur)}
  }
  def N: P[Int] = rule {
    ('0' to '9').map{c => $(c.toString).map(_.toInt)}.reduce((p1, p2) => p1 | p2)
  }
}