package jp.co.teruuu.parser.json.type;


import java.util.ArrayList;
import java.util.List;

public class JArray implements JValue {
  public List<JValue> value;

  public JArray() {
    this.value = new ArrayList<JValue>();
  }
  public JArray(List<JValue> value) {
    this.value = value;
  }
  public void add(JValue a) {
    this.value.add(a);
  }

  public boolean equals(Object other) {
    if(!(other instanceof JArray && this.value.size() == ((JArray) other).value.size())){
      return false;
    }
    for(int i = 0; i < this.value.size(); i++ ){
      if(!this.value.get(i).equals(((JArray)other).value.get(i))) {
        return false;
      }
    }
    return true;
  }
}
