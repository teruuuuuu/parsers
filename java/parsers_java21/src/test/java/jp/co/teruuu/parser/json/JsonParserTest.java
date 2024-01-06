package jp.co.teruuu.parser.json;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
        ParseResult<Json> parseResult = jsonParser.parse("{ \"array\" : [{ \"string\": \"aaaaa\", \"number\": 123, \"bool\": true, \"null\": null}] }");
        assertTrue(parseResult instanceof ParseResult.Success<Json>);
        Json value = ((ParseResult.Success<Json>) parseResult).value();
        assertEquals(value, new JObject(Map.of(
                "array",
                new JArray(List.of(
                        new JObject(Map.of(
                                "string", new JString("aaaaa"),
                                "number", new JNumber(123),
                                "bool", new JBoolean(true),
                                "null", new JNull()
                        )))
                )
        )));
    }

    @Test
    public void testPerformance() throws JsonProcessingException {
        int loopCount = 1000000;
        String jsonStr = "{ \"array\" : [{ \"string\": \"aaaaa\", \"number\": 123, \"bool\": true, \"null\": null}] }";
        for (int i = 0; i < 10; i++) {
            System.out.println(i);
            performMyParser(loopCount, jsonStr);
            performJackson(loopCount, jsonStr);
        }
        /**
         0
         MyJsonParser duration=19680ms
         JacksonParser duration=19758ms
         1
         MyJsonParser duration=19529ms
         JacksonParser duration=19113ms
         2
         MyJsonParser duration=17494ms
         JacksonParser duration=17350ms
         3
         MyJsonParser duration=16758ms
         JacksonParser duration=17319ms
         4
         MyJsonParser duration=17455ms
         JacksonParser duration=17017ms
         5
         MyJsonParser duration=17031ms
         JacksonParser duration=17390ms
         6
         MyJsonParser duration=17595ms
         JacksonParser duration=16842ms
         7
         MyJsonParser duration=17270ms
         JacksonParser duration=17426ms
         8
         MyJsonParser duration=17139ms
         JacksonParser duration=17007ms
         9
         MyJsonParser duration=17424ms
         JacksonParser duration=17422ms
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
