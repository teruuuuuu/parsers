#include "json-parser.h"

#include "gtest/gtest.h"
#include "json.h"

namespace ccombinator {

// TEST(jsontest, basic) {
//   std::cout << "start" << std::endl;
//   auto jstr = new JString("jstr1");
//   std::cout << "jstr:" << jstr->to_string() << std::endl;

//   auto jnum = new JNumber(100);
//   std::cout << "jnum:" << jnum->to_string() << std::endl;

//   auto jbool = new JBool(false);
//   std::cout << "jbool:" << jbool->to_string() << std::endl;

//   auto jnull = new JNull();
//   std::cout << "jnull:" << jnull->to_string() << std::endl;

//   auto jarray = new JArray();
//   jarray->add(new JValue(jstr));
//   jarray->add(new JValue(jnum));
//   jarray->add(new JValue(jnull));
//   auto jobj1 = new JObject();
//   jobj1->add("key", new JValue(new JNull()));
//   jarray->add(new JValue(jobj1));
//   std::cout << "jarray:" << jarray->to_string() << std::endl;

//   auto jobject = new JObject();
//   jobject->add("jarray", new JValue(jarray));
//   jobject->add("jstr", new JValue(new JString("kkkk")));
//   std::cout << "jobject:" << jobject->to_string() << std::endl;

//   std::cout << "end" << std::endl;
// }

TEST(jsontest, jstring) {
  JsonCombinator jc = JsonCombinator();
  {
    Result<JString*> result = jc.parseJStr("\"json string\" aaaa");
    ASSERT_TRUE(std::holds_alternative<Success<JString*>>(result));
    Success<JString*> success = std::get<Success<JString*>>(result);
    ASSERT_EQ("\"json string\"", success.value_->to_string());
  }
  {

    Result<JString*> result = jc.parseJStr("\"json string");
    ASSERT_TRUE(std::holds_alternative<Failure>(result));
    Failure failure = std::get<Failure>(result);
    ASSERT_EQ(12, failure.location_.column_);
  }
  {
    Result<JString*> result = jc.parseJStr("1");
    ASSERT_TRUE(std::holds_alternative<Failure>(result));
    Failure failure = std::get<Failure>(result);
    ASSERT_EQ(0, failure.location_.column_);
  }
}

TEST(jsontest, jnumber) {
  JsonCombinator jc = JsonCombinator();
  {
    Result<JNumber*> result = jc.parseJNumber("\"json string\"");
    ASSERT_TRUE(std::holds_alternative<Failure>(result));
    Failure failure = std::get<Failure>(result);
    ASSERT_EQ(0, failure.location_.column_);
  }
  {
    Result<JNumber*> result = jc.parseJNumber("0123456789");
    ASSERT_TRUE(std::holds_alternative<Success<JNumber*>>(result));
    Success<JNumber*> success = std::get<Success<JNumber*>>(result);
    ASSERT_EQ("123456789", success.value_->to_string());
  }
}

}  // namespace ccombinator