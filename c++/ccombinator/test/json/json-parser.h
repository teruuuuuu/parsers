#pragma once
#include "json.h"

namespace ccombinator {

class JsonCombinator : public CCombinator<Json> {
 public:
  explicit JsonCombinator();
  Parser<Json> gen_parser(void);
  Parser<JString*> gen_jstring_prser(void);
  Parser<JNumber*> gen_jnum_prser(void);
  Parser<JValue*> gen_jvalue_prser(void);
  Parser<JArray*> gen_jarray_prser(void);

  template <typename T>
  Result<T> parse(Parser<T> parser, std::string input) {
    input_ = input;
    auto parse_result = parser(0);

    if (std::holds_alternative<ParseSuccess<T>>(parse_result)) {
      auto p_success = std::get<ParseSuccess<T>>(parse_result);
      return Success(p_success.value_);
    } else {
      auto p_failure = std::get<ParseFailure>(parse_result);
      return Failure(Location{0, p_failure.index_}, p_failure.message_);
    }
  }

  Result<JString*> parseJStr(std::string str) {
    return parse(jstr_parser_, str);
  }

  Result<JNumber*> parseJNumber(std::string str) {
    return parse(jnum_parser_, str);
  }

  Result<JValue*> parseJValue(std::string str) {
    return parse(jvalue_parser_, str);
  }

  Result<JArray*> parseJArray(std::string str) {
    return parse(jarray_parser_, str);
  }

  template <typename T>
  Parser<T> perseFromP(Parser<T>* p) {
    return [p, *this](int index) { return (*p)(index); };
  }

 private:
  Parser<JString*>* jstr_parser2_;
  Parser<JNumber*>* jnum_parser2_;
  Parser<JString*> jstr_parser_;
  Parser<JNumber*> jnum_parser_;
  Parser<JValue*> jvalue_parser_;
  Parser<JArray*> jarray_parser_;
};

}  // namespace ccombinator