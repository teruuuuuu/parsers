package jp.co.teruuu.parser.json.type;

public class JBoolean implements JValue{
  boolean value;

  public JBoolean(boolean value) {
    this.value = value;
  }

  public boolean equals(Object other) {
    return other instanceof JBoolean && this.value == ((JBoolean) other).value;
  }

}
