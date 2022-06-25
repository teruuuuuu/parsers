#include "json-parser.h"

namespace ccombinator {

JsonCombinator::JsonCombinator() : CCombinator<Json>(gen_parser()) {
  jstr_parser_ = gen_jstring_prser();
  jnum_parser_ = gen_jnum_prser();
}

Parser<Json> JsonCombinator::gen_parser(void) {
  // Parser<JObject*> p = map(pstr(""), [](std::string a) {
  //     JObject* ret = new JObject();
  //     return ret;
  // });

  // return map(pstr(""), [](std::string a) {
  //     JObject* ret = new JObject();
  //     return ret;
  // });
  return nullptr;
}

Parser<JString*> JsonCombinator::gen_jstring_prser(void) {
  auto p1 = pstr("\"");
  auto p2 = repeat0(except('"'));

  std::function<Parser<std::string>(std::string)> f1 = [&, p2](std::string a) {
    std::function<std::string(std::vector<std::string>)> f =
        [](std::vector<std::string> list) {
          std::string ret = "";
          for (std::string b : list) {
            ret += b;
          }
          return ret;
        };
    return map(p2, f);
  };
  std::function<Parser<std::string>(std::string)> f2 = [&, p1](std::string a) {
    std::function<std::string(std::string)> f = [a](std::string b) {
      return a;
    };
    return map(p1, f);
  };
  std::function<JString*(std::string)> f3 = [](std::string a) {
    return new JString(std::move(a));
  };
  return map(flatMap(flatMap(p1, f1), f2), f3);
}

Parser<JNumber*> JsonCombinator::gen_jnum_prser(void) {
  std::vector<char> numcs = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
  std::function<JNumber*(std::vector<std::string>)> f1 =
      [](std::vector<std::string> list) {
        int ret = 0;
        for (std::string c : list) {
          ret *= 10;
          ret += stoi(c);
        }
        return new JNumber(ret);
      };

  return map(repeat1(pset(numcs)), f1);
}

}  // namespace ccombinator