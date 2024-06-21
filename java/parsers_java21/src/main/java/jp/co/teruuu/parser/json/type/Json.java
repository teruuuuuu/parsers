package jp.co.teruuu.parser.json.type;

public sealed interface Json
        permits JArray, JBoolean, JNull, JNumber, JObject, JString {
}
