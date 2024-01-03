package jp.co.teruuu.parser.json.type;

import java.util.List;

public record JArray(List<JValue> value) implements JValue, Json {
}
