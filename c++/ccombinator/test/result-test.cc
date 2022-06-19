#include <variant>

#include "ccombinator.h"
#include "gtest/gtest.h"

namespace ccombinator {

class StrCombinator : public CCombinator<std::string> {
 public:
  explicit StrCombinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = optional(pstr("aaaa"));
    std::function<std::string(std::optional<std::string>)> f1 =
        [](std::optional<std::string> optStr) {
          if (optStr) {
            return optStr.value();
          } else {
            return static_cast<std::string>("not much");
          }
        };
    Parser<std::optional<std::string>> p2 = optional(pstr("aaaa"));
    return map(p1, f1);
  }
};

TEST(ccombinator, parser) {
  auto scombinator = StrCombinator();
  auto result = scombinator.parse("aaaa");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;
}

class OrCombinator : public CCombinator<std::string> {
 public:
  explicit OrCombinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("aaa");
    auto p2 = pstr("bbb");
    return orp(p1, p2);
  }
};

TEST(ccombinator, orp) {
  auto scombinator = OrCombinator();
  auto result = scombinator.parse("bbb");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;
}

class AndCombinator : public CCombinator<std::string> {
 public:
  explicit AndCombinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("aaa");
    auto p2 = pstr("bbb");
    std::function<std::string(std::tuple<std::string, std::string>)> f1 =
        [](std::tuple<std::string, std::string> input) {
          return std::get<0>(input) + std::get<1>(input);
        };
    auto p3 = andp(p1, p2);
    return map(p3, f1);
  }
};

TEST(ccombinator, andp) {
  auto scombinator = AndCombinator();
  auto result = scombinator.parse("aaabbb");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;
}

class Repeat0Combinator : public CCombinator<std::string> {
 public:
  explicit Repeat0Combinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("abc");
    auto p2 = repeat0(p1);
    std::function<std::string(std::vector<std::string>)> f1 =
        [](std::vector<std::string> input) {
          std::string ret = "";
          for (std::string a : input) {
            ret += a;
          }
          return ret;
        };
    return map(p2, f1);
  }
};

TEST(ccombinator, repeat0parser) {
  auto scombinator = Repeat0Combinator();
  auto result = scombinator.parse("abcabc");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;

  auto result2 = scombinator.parse("bcabc");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result2));
  auto result_success2 = std::get<Success<std::string>>(result2);
  std::cout << result_success2.value_ << std::endl;
}

class Repeat1Combinator : public CCombinator<std::string> {
 public:
  explicit Repeat1Combinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("abc");
    auto p2 = repeat1(p1);
    std::function<std::string(std::vector<std::string>)> f1 =
        [](std::vector<std::string> input) {
          std::string ret = "";
          for (std::string a : input) {
            ret += a;
          }
          return ret;
        };
    return map(p2, f1);
  }
};

TEST(ccombinator, repeat1parser) {
  auto scombinator = Repeat1Combinator();
  auto result = scombinator.parse("abcabc");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;

  auto result2 = scombinator.parse("bcabc");
  ASSERT_TRUE(std::holds_alternative<Failure>(result2));
}

class FlatMapCombinator : public CCombinator<std::string> {
 public:
  explicit FlatMapCombinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("abc");
    auto p2 = pstr("def");

    // autoだと推論できない
    std::function<Parser<std::string>(std::string)> f2 = [&,
                                                          p2](std::string a) {
      std::function<std::string(std::string)> f = [a](std::string b) {
        return a + b;
      };
      return map(p2, f);
    };

    return flatMap(p1, f2);
  }
};

TEST(ccombinator, flatmapparser) {
  auto scombinator = FlatMapCombinator();
  auto result = scombinator.parse("abcdef");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;
}

class ExceptCombinator : public CCombinator<std::string> {
 public:
  explicit ExceptCombinator() : CCombinator<std::string>(gen_parser()) {}

  Parser<std::string> gen_parser(void) {
    auto p1 = pstr("\"");
    auto p2 = repeat0(except('"'));

    std::function<Parser<std::string>(std::string)> f12 = [&,
                                                           p2](std::string a) {
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
    std::function<Parser<std::string>(std::string)> f21 = [&,
                                                           p1](std::string a) {
      std::function<std::string(std::string)> f = [a](std::string b) {
        return a;
      };
      return map(p1, f);
    };
    return flatMap(flatMap(p1, f12), f21);
  }
};

TEST(ccombinator, exceptcombinator) {
  auto scombinator = ExceptCombinator();
  auto result = scombinator.parse("\"string\"");
  ASSERT_TRUE(std::holds_alternative<Success<std::string>>(result));
  auto result_success = std::get<Success<std::string>>(result);
  std::cout << result_success.value_ << std::endl;
}

class SetCombinator : public CCombinator<int> {
 public:
  explicit SetCombinator() : CCombinator<int>(gen_parser()) {}

  Parser<int> gen_parser(void) {
    std::vector<char> numcs = {'0', '1', '2', '3', '4',
                               '5', '6', '7', '8', '9'};
    std::function<int(std::vector<std::string>)> f1 =
        [](std::vector<std::string> list) {
          int ret = 0;
          for (std::string c : list) {
            ret *= 10;
            ret += stoi(c);
          }
          return ret;
        };

    return map(repeat1(pset(numcs)), f1);
  }
};

TEST(ccombinator, setcombinator) {
  auto scombinator = SetCombinator();
  auto result = scombinator.parse("12345");
  ASSERT_TRUE(std::holds_alternative<Success<int>>(result));
  auto result_success = std::get<Success<int>>(result);
  std::cout << result_success.value_ << std::endl;
}

class JValue {
  std::variant<std::string, double, nullptr_t, std::map<std::string, JValue*>,
               std::vector<JValue*>>
      value_;
};

TEST(ccombinator, json) {}

}  // namespace ccombinator
