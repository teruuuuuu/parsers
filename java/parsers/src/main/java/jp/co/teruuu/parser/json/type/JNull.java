package jp.co.teruuu.parser.json.type;

public class JNull implements JValue {
  public JNull(){}

  public boolean equals(Object other){
    return other instanceof JNull;
  }
}
