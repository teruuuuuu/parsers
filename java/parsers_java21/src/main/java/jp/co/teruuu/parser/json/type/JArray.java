package jp.co.teruuu.parser.json.type;

import java.util.List;

public record JArray(List<Json> value) implements Json {
}
