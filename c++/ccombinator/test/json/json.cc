#include "json.h"

namespace ccombinator {

JValue::JValue(JString* val) {
  type_ = Jstr;
  jst = val;
}
JValue::JValue(JNumber* val) {
  type_ = Jnum;
  jnum = val;
}
JValue::JValue(JNull* val) {
  type_ = Jnull;
  jnull = val;
}
JValue::JValue(JObject* val) {
  type_ = Jobj;
  jobject = val;
}
JValue::JValue(JArray* val) {
  type_ = Jarray;
  jarray = val;
}
JValue::~JValue() {
  if (type_ == Jstr) {
    delete jst;
  } else if (type_ == Jnum) {
    delete jnum;
  } else if (type_ == Jnull) {
    delete jnull;
  } else if (type_ == Jobj) {
    delete jobject;
  } else if (type_ == Jarray) {
    delete jarray;
  } else {
  }
}
std::string JValue::to_string() {
  if (type_ == Jstr) {
    return jst->to_string();
  } else if (type_ == Jnum) {
    return jnum->to_string();
  } else if (type_ == Jnull) {
    return jnull->to_string();
  } else if (type_ == Jobj) {
    return jobject->to_string();
  } else if (type_ == Jarray) {
    return jarray->to_string();
  } else {
    return "";
  }
}

JObject::JObject() {}
JObject::~JObject() {
  for (auto& [k, v] : value_) {
    delete v;
  }
}

void JObject::add(std::string k, JValue* v) { value_.emplace(k, v); }

std::string JObject::to_string() {
  int len = value_.size();
  std::string ret = "{";
  int i = 0;
  for (auto& [k, v] : value_) {
    ret += "\"" + k + "\"" + ":" + v->to_string();
    if (i <= len - 2) {
      ret += ",";
    }
    i++;
  }
  ret += "}";
  return ret;
}

JArray::JArray() {}
JArray::~JArray() {
  for (JValue* v : value_) {
    delete v;
  }
}
void JArray::add(JValue* v) { value_.push_back(v); }
std::string JArray::to_string() {
  int len = value_.size();
  std::string ret = "[";
  for (int i = 0; i <= len - 2; i++) {
    ret += value_.at(i)->to_string();
    ret += ",";
  }
  if (len > 0) {
    ret += value_.at(len - 1)->to_string();
  }

  ret += "]";
  return ret;
}

JString::JString(std::string&& value) : value_(std::move(value)) {}
std::string JString::to_string() { return "\"" + value_ + "\""; }

JNumber::JNumber(std::int32_t value) : value_(value) {}
std::string JNumber::to_string() { return std::to_string(value_); }

JBool::JBool(bool value) : value_(value) {}
std::string JBool::to_string() { return value_ ? "true" : "false"; }

std::string JNull::to_string() { return "null"; }

}  // namespace ccombinator