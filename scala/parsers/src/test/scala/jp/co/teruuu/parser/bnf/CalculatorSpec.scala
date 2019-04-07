package jp.co.teruuu.parser.bnf

import jp.co.teruuu.parser.bnf.parser.Success
import org.scalatest.FunSpec;

class CalculatorSpec extends FunSpec {

  val parser = calculator.E
  describe("A Calculator") {
    var input = ""
    it("should parse correct expressions") {
      input = "1+2*3"
      assert(parser(input) == Success(7, ""))
      input = "1+5*3/4"
      assert(parser(input) == Success(4, ""))
      input = "(1+5)*3/2"
      assert(parser(input) == Success(9, ""))
    }
    it("cannot parse incorrect expressions, which ends with unexpected EOF") {
      input = "1+ "
      assert(parser(input) == Success(1, "+ "))
    }

    it("cannot parse incorrect expressions, which contains spaces") {
      input = "(1-5) *3/2"
      assert(parser(input) == Success(-4, " *3/2"))
    }

    it("cannot parse incorrect expressions,") {
      input = "11+5"
      assert(parser(input) ==Success(16, ""))
    }
  }
}
