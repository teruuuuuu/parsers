package jp.co.teruuu.parser.json;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jp.co.teruuu.common.Either;
import jp.co.teruuu.parser.common.ParseResult;
import jp.co.teruuu.parser.common.Parser;
import jp.co.teruuu.parser.json.type.*;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class JsonParserTest {

    @Test
    public void test() {
        Parser<Json> jsonParser = new JsonParser();
        assertTrue(jsonParser.parse(" \"string\"") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" 123.456") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" true") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" false") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" null") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" []") instanceof ParseResult.Success<Json>);
        assertTrue(jsonParser.parse(" {}") instanceof ParseResult.Success<Json>);

        ParseResult<Json> parseResult = jsonParser.parse(" { \"array\" : [{ \"string\": \"aaaaa\", \"numberInt\": 123, \"numberDouble\": -123.456, \"bool\": true, \"null\": null}] }");
        assertTrue(parseResult instanceof ParseResult.Success<Json>);
        Json value = ((ParseResult.Success<Json>) parseResult).value();
        assertEquals(value, new JObject(Map.of(
                "array",
                new JArray(List.of(
                        new JObject(Map.of(
                                "string", new JString("aaaaa"),
                                "numberInt", new JNumber(new Either.Left<>(123)),
                                "numberDouble", new JNumber(new Either.Right<>(-123.456)),
                                "bool", new JBoolean(true),
                                "null", new JNull()
                        )))
                )
        )));
    }

    @Test
    public void testPerformance() throws JsonProcessingException {

        Double.valueOf("123.");
        int loopCount = 1000000;
        String jsonStr = " { \"array\" : [{ \"string\": \"aaaaa\", \"numberInt\": 123, \"numberDouble\": -123.456, \"bool\": true, \"null\": null}] }";
        for (int i = 0; i < 10; i++) {
            System.out.println(i);
            performMyParser(loopCount, jsonStr);
            performJackson(loopCount, jsonStr);
        }
        /**
         0
         MyJsonParser duration=28057ms
         JacksonParser duration=28061ms
         1
         MyJsonParser duration=28098ms
         JacksonParser duration=28076ms
         2
         MyJsonParser duration=25688ms
         JacksonParser duration=25820ms
         3
         MyJsonParser duration=25702ms
         JacksonParser duration=25819ms
         4
         MyJsonParser duration=25750ms
         JacksonParser duration=25940ms
         5
         MyJsonParser duration=25705ms
         JacksonParser duration=25827ms
         6
         MyJsonParser duration=25700ms
         JacksonParser duration=25829ms
         7
         MyJsonParser duration=25689ms
         JacksonParser duration=25841ms
         8
         MyJsonParser duration=25773ms
         JacksonParser duration=25840ms
         9
         MyJsonParser duration=25692ms
         JacksonParser duration=25830ms
         **/
    }

    private void performMyParser(int loopCount, String jsonStr) {
        Parser<Json> jsonParser = new JsonParser();
        jsonParser.parse(jsonStr);
        LocalDateTime start;
        LocalDateTime end;

        start = LocalDateTime.now();
        for (int i = 0; i < loopCount; i++) {
            jsonParser.parse(jsonStr);
        }
        end = LocalDateTime.now();
        System.out.println(String.format("MyJsonParser duration=%dms", Duration.between(start, end).toMillis()));
    }

    private void performJackson(int loopCount, String jsonStr) throws JsonProcessingException {
        Parser<Json> jsonParser = new JsonParser();
        jsonParser.parse(jsonStr);
        LocalDateTime start;
        LocalDateTime end;

        ObjectMapper mapper = new ObjectMapper();
        mapper.readValue(jsonStr, Map.class);
        start = LocalDateTime.now();
        for (int i = 0; i < loopCount; i++) {
            jsonParser.parse(jsonStr);
        }
        end = LocalDateTime.now();
        System.out.println(String.format("JacksonParser duration=%dms", Duration.between(start, end).toMillis()));
    }
}
