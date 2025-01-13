use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum JValue {
    JObject(HashMap<String, JValue>),
    JArray(Vec<JValue>),
    JString(String),
    JNumber(i64),
    JBool(bool),
    JNull
}