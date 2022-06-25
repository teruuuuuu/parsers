#pragma once
#include <map>
#include <memory>
#include <variant>
#include <vector>

#include "ccombinator.h"

namespace ccombinator {

class JValue;
class JObject;
class JArray;
class JString;
class JNumber;
class JBool;
class JNull;

using Json = std::variant<JObject*, JArray*>;

class JValue {
 public:
  enum Type : uint16_t { Jstr, Jnum, Jnull, Jobj, Jarray };
  JValue(JString* val);
  JValue(JNumber* val);
  JValue(JNull* val);
  JValue(JObject* val);
  JValue(JArray* val);
  ~JValue();
  std::string to_string();

 private:
  Type type_;
  JString* jst;
  JNumber* jnum;
  JNull* jnull;
  JObject* jobject;
  JArray* jarray;
};

class JObject {
 public:
  JObject();
  ~JObject();
  void add(std::string k, JValue* v);
  std::string to_string();

 private:
  std::map<std::string, JValue*> value_;
};

class JArray {
 public:
  JArray();
  ~JArray();
  void add(JValue* v);
  std::string to_string();

 private:
  std::vector<JValue*> value_;
};

class JString {
 public:
  JString(std::string&& value);
  std::string to_string();

 private:
  std::string value_;
};

class JNumber {
 public:
  JNumber(std::int32_t value);
  std::string to_string();

 private:
  std::int32_t value_;
};

class JBool {
 public:
  JBool(bool value);
  std::string to_string();

 private:
  bool value_;
};

class JNull {
 public:
  std::string to_string();
};


}  // namespace ccombinator