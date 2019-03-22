package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.JValue;

public interface JParser<T> extends Parser<T> {

  default JParser<JValue> or(JParser<JValue> rhs) {
    return new JOrParser(this, rhs);
  }
  static JParser<JValue> bool() {
    return new JBoolParser();
  }
  static JParser<JValue> nu() {
    return new JNullParser();
  }
  static JParser<JValue> number() {
    return new JNumberParser();
  }
  static JParser<JValue> string() {
    return new JStringParser();
  }

  static JParser<JValue> object() {
    return new JObjectParser();
  }
  static JParser<JValue> array() {
    return new JArrayParser();
  }
  static JParser<JValue> value() {
    return JParser.bool().or(JParser.nu()).or(JParser.number()).or(JParser.string()).or(JParser.array()).or(JParser.object());
  }
}