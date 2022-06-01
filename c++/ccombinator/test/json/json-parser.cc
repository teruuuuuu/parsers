#include "json-parser.h"

namespace ccombinator {

std::string join(const std::vector<std::string>& v, const char* delim = 0) {
  std::string s;
  if (!v.empty()) {
    s += v[0];
    for (decltype(v.size()) i = 1, c = v.size(); i < c; ++i) {
      if (delim) s += delim;
      s += v[i];
    }
  }
  return s;
}

JsonCombinator::JsonCombinator() : CCombinator<Json>(gen_parser()) {
  std::cout << "&jstr_parser_:" << &jstr_parser_ << std::endl;
  jvalue_parser_ = gen_jvalue_prser();
  jarray_parser_ = gen_jarray_prser();
  jstr_parser_ = gen_jstring_prser();
  jnum_parser_ = gen_jnum_prser();
  std::cout << "&jstr_parser_:" << &jstr_parser_ << std::endl;
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
        [](std::vector<std::string> list) { return join(list, nullptr); };
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

Parser<JValue*> JsonCombinator::gen_jvalue_prser(void) {
  std::function<JValue*(JString*)> jstr_f = [](JString* a) {
    return new JValue(a);
  };

  std::function<JValue*(JNumber*)> jnum_f = [](JNumber* a) {
    return new JValue(a);
  };

  std::function<JValue*(JArray*)> jarray_f = [](JArray* a) {
    return new JValue(a);
  };

  return orp(orp(map(perseFromP(&jstr_parser_), jstr_f),
                 map(perseFromP(&jnum_parser_), jnum_f)),
             map(perseFromP(&jarray_parser_), jarray_f));
}

Parser<JArray*> JsonCombinator::gen_jarray_prser(void) {
  Parser<JValue*> jvalue_parser = perseFromP(&jvalue_parser_);

//   std::function<JArray*(std::vector<JValue*>)> f1 =
//       [](std::vector<JValue*> list) {
//         JArray* jarray = new JArray();
//         for (auto a : list) {
//           jarray->add(a);
//         }
//         return jarray;
//       };
  auto jvalue_seq_parser = repeat0By(jvalue_parser, pstr(","));
//   Parser<JArray*> jvalue_seq_parser =
//       map(repeat0By(jvalue_parser, pstr(",")), f1);

//   std::function<Parser<JArray*>(std::string)> f2 =
//       [&, jvalue_seq_parser](std::string a) {
//         std::function<JArray*(JArray*)> func = [a](JArray* jarray) {
//           return jarray;
//         };
//         return map(jvalue_seq_parser, func);
//       };

  //   std::function<Parser<JArray*>(Parser<std::string>)> dump_back =
  //       [&](Parser<JArray*> front, Parser<std::string> back) {
  //         std::function<JArray*(std::tuple < JArray*, std::string)> func =
  //             [](std::tuple < JArray*, std::string a) {
  //               return std::get<JArray*>(a);
  //             };
  //             return map(andp(front, back), func);
  //       };

  return nullptr;
//   return flatMap(pstr("["), f2);
  //   return dump_back(flatMap(pstr("["), f2), pstr("]"));

  //   std::function<Parser<JValue*>(std::string)> f1 =
  //       [&, jvalue_parser](std::string a) {
  //         std::function<JValue*(JValue*)> func = [a](JValue* jv) { return jv;
  //         }; return map(jvalue_parser, func);
  //       };
  //   Parser<std::vector<JValue*>> jvalue_parser2 = repeat0(flatMap(pstr(","),
  //   f1));

  //   std::function<Parser<JArray*>(JValue*)> f2 = [&, jvalue_parser2](JValue*
  //   a) {
  //     std::function<JArray*(std::vector<JValue*>)> func =
  //         [a](std::vector<JValue*> list) {
  //           JArray* jarray = new JArray();
  //           jarray->add(a);
  //           for (auto b : list) {
  //             jarray->add(b);
  //           }
  //           return jarray;
  //         };
  //     return map(jvalue_parser2, func);
  //   };
  //   Parser<JArray*> jvalue_parser3 = flatMap(jvalue_parser, f2);
  //   std::function<JArray*(std::optional<JArray*>)> f3 =
  //       [](std::optional<JArray*> jarrayOpt) {
  //         return jarrayOpt.value_or(new JArray());
  //       };
  //   Parser<JArray*> jvalue_parser_f = map(optional(jvalue_parser3), f3);

  //   std::function<Parser<JArray*>(std::string)> f4 =
  //       [&, jvalue_parser_f](std::string a) { return jvalue_parser_f; };

  //   std::function<Parser<JArray*>(JArray*)> f5 = [&](JArray* a) {
  //     std::function<JArray*(std::string)> func = [a](std::string b) { return
  //     a; }; return map(pstr("]"), func);
  //   };

  //   return flatMap(flatMap(pstr("["), f4), f5);
}

}  // namespace ccombinator