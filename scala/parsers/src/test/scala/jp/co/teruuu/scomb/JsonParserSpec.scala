package jp.co.teruuu.scomb

import com.github.kmizu.scomb.{Location, Result}
import org.scalatest.{DiagrammedAssertions, FunSpec}

class JsonParserSpec extends FunSpec with DiagrammedAssertions {
  import jp.co.teruuu.scomb.JsonParser._

  describe("JSONParser can parse basic values") {
    it("null") {
      assert(Some(JNull) == parse("null").value)
      assert(Some(JNull) == parse(" null").value)
      assert(Some(JNull) == parse(" null ").value)
      assert(Some(JNull) == parse("null  ").value)
      assert(Some(JNull) == parse("  null").value)
    }
    it("boolean") {
      assert(Some(JBoolean(true)) == parse("true").value)
      assert(Some(JBoolean(false)) == parse("false").value)
      assert(Some(JBoolean(true)) == parse("true ").value)
      assert(Some(JBoolean(true)) == parse(" true").value)
      assert(Some(JBoolean(true)) == parse(" true ").value)
      assert(Some(JBoolean(false)) == parse("false ").value)
      assert(Some(JBoolean(false)) == parse(" false").value)
      assert(Some(JBoolean(false)) == parse(" false ").value)
    }
    it("number") {
      assert(Some(JNumber(0)) == parse("0").value)
      assert(Some(JNumber(0)) == parse(" 0").value)
      assert(Some(JNumber(0)) == parse("0 ").value)
      assert(Some(JNumber(0)) == parse(" 0 ").value)
      assert(Some(JNumber(200)) == parse("200").value)
      assert(Some(JNumber(200)) == parse(" 200").value)
      assert(Some(JNumber(200)) == parse("200 ").value)
      assert(Some(JNumber(200)) == parse(" 200 ").value)
      assert(Some(JNumber(300)) == parse("300").value)
      assert(Some(JNumber(300)) == parse(" 300").value)
      assert(Some(JNumber(300)) == parse("300 ").value)
      assert(Some(JNumber(300)) == parse(" 300 ").value)
      assert(Some(JNumber(-300)) == parse(" -300 ").value)
      assert(Some(JNumber(-300.456)) == parse(" -300.456 ").value)
      assert(Some(JNumber(300.456)) == parse(" +300.456 ").value)
      assert(Some(JNumber(-30045600)) == parse(" -300.456E+5 ").value)
      assert(Some(JNumber(30045.6)) == parse(" +300.456E2 ").value)
      assert(Some(JNumber(30.0456)) == parse(" +300.456E-1 ").value)
    }

    it("string") {
      assert(Some(JString("")) == parse("\"\"").value)
    }
  }

  describe("A JsonParser") {
    it("should parse an object") {
      assert(Some(JObject()) == parse("{}").value)
      assert(Some(JObject("k" -> JObject())) == parse("{\"k\":{}}").value)
      assert(Some(JObject("x" -> JNumber(100), "y" -> JNumber(200))) == parse("{\"x\": 100, \"y\": 200}").value)
    }
    it("should parse an array") {
      assert(Some(JArray()) == parse("[]").value)
      assert(Some(JArray(JArray())) == parse("[[]]").value)
      assert(Some(JArray(JNumber(1), JNumber(2), JNumber(3))) == parse("[1, 2, 3]").value)
      assert(Some(JArray(JObject())) == parse("[{}]").value)
    }
  }

  describe("The JsonParser") {
    it("cannot parse incorrect object") {
      val failure = parse("{").asInstanceOf[Result.Failure]
      assert(Location(1, 2) == failure.location)
      assert("""expected:`}` actual:EOF in <RBRACE>""" == failure.message)
    }
  }

  describe("The JsonParser") {
    it("cannot parse incorrect array") {
      val failure = parse("[1, 2, ]").asInstanceOf[Result.Failure]
      assert(Location(1, 6) == failure.location)
      assert("""expected:`]` actual:`,` in <rbracket>""" == failure.message)
    }
  }
}
