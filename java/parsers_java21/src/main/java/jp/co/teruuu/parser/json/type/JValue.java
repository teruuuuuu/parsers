package jp.co.teruuu.parser.json.type;

public sealed interface JValue
        permits JArray, JBoolean, JNull, JNumber, JObject, JString {
}
