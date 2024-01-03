package jp.co.teruuu.parser.json.type;

import java.util.Map;

public record JObject(Map<String, JValue> map) implements JValue, Json { }
