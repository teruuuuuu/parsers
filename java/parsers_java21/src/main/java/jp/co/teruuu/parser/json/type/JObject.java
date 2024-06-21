package jp.co.teruuu.parser.json.type;

import java.util.Map;

public record JObject(Map<String, Json> map) implements Json { }
