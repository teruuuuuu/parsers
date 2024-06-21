package jp.co.teruuu.parser.json.type;

import java.util.Map;

public class JMembers implements JValue {
  Map<JString, JValue> value;

  public JMembers(Map<JString, JValue> value) {
    this.value = value;
  }
}
