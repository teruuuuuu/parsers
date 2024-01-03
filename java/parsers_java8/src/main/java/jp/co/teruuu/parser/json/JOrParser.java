package jp.co.teruuu.parser.json;

import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;

public class JOrParser<X> implements JParser<X> {
  private JParser<X> lhs;
  private Parser<X> rhs;
  public JOrParser(JParser<X> lhs, JParser<X> rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public ParseResult<X> parse(String input) {
    ParseResult<X> lresult = lhs.parse(input);
    if(lresult instanceof ParseResult.Failure<?>) {
      return rhs.parse(input);
    } else {
      return lresult;
    }
  }
}
