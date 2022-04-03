#include <functional>
#include <iostream>
#include <optional>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

namespace ccombinator {

class Location {
 public:
  int line_;
  int column_;
  Location(int line, int column)
      : line_(line), column_(column) {}
};

template <typename T>
class Success {
 public:
  T value_;
  Success(T value) : value_(value) {}
};

class Failure {
 public:
  Location location_;
  std::string message_;
  Failure(Location location, std::string message)
      : location_(location), message_(message) {}
};

template <typename T>
using Result = std::variant<Success<T>, Failure>;

template <typename T>
class ParseSuccess {
 public:
  int index_;
  T value_;
  ParseSuccess(int index, T value) : index_(index), value_(value) {}
};

class ParseFailure {
 public:
  int index_;
  std::string message_;
  ParseFailure(int index, std::string message)
      : index_(index), message_(message) {}
};

template <typename T>
using ParseResult = std::variant<ParseSuccess<T>, ParseFailure>;

template <typename F>
using Parser = std::function<ParseResult<F>(int index)>;

template <typename T>
class CCombinator {
 public:
  Parser<T> root_parser_;
  std::string input_;
  CCombinator(Parser<T> root_parser)
      : root_parser_(root_parser), input_("") {}

  Result<T> parse(std::string input) {
    input_ = input;
    auto parse_result = root_parser_(0);

    if (std::holds_alternative<ParseSuccess<T>>(parse_result)) {
      auto p_success = std::get<ParseSuccess<T>>(parse_result);
      return Success(p_success.value_);
    } else {
      auto p_failure = std::get<ParseFailure>(parse_result);
      return Failure(
          Location{0, p_failure.index_},
          p_failure.message_);
    }
  }

  bool isEOF(int index) {
    return input_.size() == index;
  }

  Parser<std::string> pstr(std::string literal) {
    return [this, literal](int index) {
      std::string input = input_.substr(index);
      int len = literal.size();
      if (input.size() >= len &&
          input.substr(0, len) == literal) {
        return static_cast<ParseResult<std::string>>(
            ParseSuccess(index + len, literal));
      } else {
        return static_cast<ParseResult<std::string>>(
            ParseFailure(index, "expect " + literal));
      }
    };
  }

  Parser<std::string> pset(std::vector<char> clist) {
    return [this, clist](int index) {
      if (isEOF(index)) {
        return static_cast<ParseResult<std::string>>(
            ParseFailure(index, "unexpected EOF"));
      } else {
        char cur = input_.at(index);
        for (char c : clist) {
          if (cur == c) {
            return static_cast<ParseResult<std::string>>(
                ParseSuccess(index + 1, input_.substr(index, 1)));
          }
        }
        return static_cast<ParseResult<std::string>>(
            ParseFailure(index, "unexpected char " + input_.at(index)));
      }
    };
  }

  Parser<std::string> except(char c) {
    return [this, c](int index) {
      if (isEOF(index)) {
        return static_cast<ParseResult<std::string>>(
            ParseFailure(index, "unexpected EOF"));
      } else if (input_.at(index) != c) {
        return static_cast<ParseResult<std::string>>(
            ParseSuccess(index + 1, input_.substr(index, 1)));
      } else {
        return static_cast<ParseResult<std::string>>(
            ParseFailure(index, "unexpected char " + input_.at(index)));
      }
    };
  }

  template <typename S, typename V>
  Parser<V> map(Parser<S> parser, std::function<V(S)> f) {
    return [this, parser, f](int index) {
      auto parse_result = parser(index);
      if (std::holds_alternative<ParseSuccess<S>>(parse_result)) {
        auto p_success = std::get<ParseSuccess<S>>(parse_result);
        return static_cast<ParseResult<V>>(ParseSuccess(p_success.index_, f(p_success.value_)));
      } else {
        auto p_failure = std::get<ParseFailure>(parse_result);
        return static_cast<ParseResult<V>>(
            ParseFailure(p_failure.index_, p_failure.message_));
      }
    };
  }

  template <typename S, typename V>
  Parser<V> flatMap(Parser<S> parser, std::function<Parser<V>(S)> f) {
    return [this, parser, f](int index) {
      auto parse_result = parser(index);
      if (std::holds_alternative<ParseSuccess<S>>(parse_result)) {
        auto p_success = std::get<ParseSuccess<S>>(parse_result);
        return static_cast<ParseResult<V>>(
            f(p_success.value_)(p_success.index_));
      } else {
        auto p_failure = std::get<ParseFailure>(parse_result);
        return static_cast<ParseResult<V>>(
            ParseFailure(p_failure.index_, p_failure.message_));
      }
    };
  }

  template <typename V>
  Parser<std::optional<V>> optional(Parser<V> parser) {
    return [this, parser](int index) {
      auto parse_result = parser(index);
      if (std::holds_alternative<ParseSuccess<V>>(parse_result)) {
        auto p_success = std::get<ParseSuccess<V>>(parse_result);
        return static_cast<ParseResult<std::optional<V>>>(
            ParseSuccess(p_success.index_, std::optional<V>(p_success.value_)));
      } else {
        auto p_failure = std::get<ParseFailure>(parse_result);
        return static_cast<ParseResult<std::optional<V>>>(
            ParseSuccess(p_failure.index_, std::optional<V>()));
      }
    };
  }

  template <typename V>
  Parser<V> orp(Parser<V> parser1, Parser<V> parser2) {
    return [this, parser1, parser2](int index) {
      auto parse_result1 = parser1(index);
      if (std::holds_alternative<ParseSuccess<V>>(parse_result1)) {
        return static_cast<ParseResult<V>>(
            std::get<ParseSuccess<V>>(parse_result1));
      } else {
        auto parse_result2 = parser2(index);
        if (std::holds_alternative<ParseSuccess<V>>(parse_result2)) {
          return static_cast<ParseResult<V>>(
              std::get<ParseSuccess<V>>(parse_result2));
        } else {
          auto p_failure = std::get<ParseFailure>(parse_result2);
          return static_cast<ParseResult<V>>(
              ParseFailure(p_failure.index_, p_failure.message_));
        }
      }
    };
  }

  template <typename V, typename U>
  Parser<std::tuple<V, U>> andp(Parser<V> parser1, Parser<U> parser2) {
    return [this, parser1, parser2](int index) {
      auto parse_result1 = parser1(index);
      if (std::holds_alternative<ParseSuccess<V>>(parse_result1)) {
        auto parse_success1 = std::get<ParseSuccess<V>>(parse_result1);
        auto parse_result2 = parser2(parse_success1.index_);

        if (std::holds_alternative<ParseSuccess<V>>(parse_result2)) {
          auto parse_success2 = std::get<ParseSuccess<U>>(parse_result2);

          return static_cast<ParseResult<std::tuple<V, U>>>(
              ParseSuccess(parse_success2.index_,
                           std::tuple<V, U>(parse_success1.value_, parse_success2.value_)));
        } else {
          auto p_failure2 = std::get<ParseFailure>(parse_result2);
          return static_cast<ParseResult<std::tuple<V, U>>>(
              ParseFailure(p_failure2.index_, p_failure2.message_));
        }
      } else {
        auto p_failure1 = std::get<ParseFailure>(parse_result1);
        return static_cast<ParseResult<std::tuple<V, U>>>(
            ParseFailure(p_failure1.index_, p_failure1.message_));
      }
    };
  }

  template <typename V>
  Parser<std::vector<V>> repeat0(Parser<V> parser) {
    return [this, parser](int index) {
      std::vector<V> value;
      while (true) {
        auto parse_result = parser(index);
        if (std::holds_alternative<ParseSuccess<V>>(parse_result)) {
          auto parse_success = std::get<ParseSuccess<V>>(parse_result);
          value.push_back(parse_success.value_);
          if (index == parse_success.index_) {
            break;
          } else {
            index = parse_success.index_;
          }
        } else {
          break;
        }
      }
      return static_cast<ParseResult<std::vector<V>>>(
          ParseSuccess(index, value));
    };
  }

  template <typename V>
  Parser<std::vector<V>> repeat1(Parser<V> parser) {
    return [this, parser](int index) {
      auto parse_result1 = parser(index);
      if (std::holds_alternative<ParseSuccess<V>>(parse_result1)) {
        auto parse_success1 = std::get<ParseSuccess<V>>(parse_result1);
        auto parse_success2 = std::get<ParseSuccess<std::vector<V>>>(
            repeat0(parser)(parse_success1.index_));
        auto value = parse_success2.value_;
        value.insert(value.begin(), 1, parse_success1.value_);
        return static_cast<ParseResult<std::vector<V>>>(
            ParseSuccess(parse_success2.index_, value));
      } else {
        auto p_failure1 = std::get<ParseFailure>(parse_result1);
        return static_cast<ParseResult<std::vector<V>>>(
            ParseFailure(p_failure1.index_, p_failure1.message_));
      }
    };
  }
};

}  // namespace ccombinator
