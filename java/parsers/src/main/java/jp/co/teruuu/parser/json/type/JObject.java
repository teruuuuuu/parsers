package jp.co.teruuu.parser.json.type;

import java.util.Map;

public class JObject {
  Map<JString, JValue> map;

  public JObject(Map<JString, JValue> map) {
    this.map = map;
  }

  @Override
  public boolean equals(Object other) {
    if(!(other instanceof JObject) || this.map.keySet().size() != ((JObject) other).map.keySet().size()) {
      return false;
    }
    for(JString key: map.keySet()) {
      if(!((JObject) other).map.containsKey(key) || !this.map.get(key).equals(((JObject) other).map.get(key))) {
        return false;
      }
    }
    return true;
  }
}
