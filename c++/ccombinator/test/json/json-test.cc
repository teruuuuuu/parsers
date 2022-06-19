#include "json.h"

#include "gtest/gtest.h"

namespace ccombinator {

TEST(jsontest, basic) {
  std::cout << "start" << std::endl;
  auto jstr = new JString("jstr1");
  std::cout << "jstr:" << jstr->to_string() << std::endl;

  auto jnum = new JNumber(100);
  std::cout << "jnum:" << jnum->to_string() << std::endl;

  auto jbool = new JBool(false);
  std::cout << "jbool:" << jbool->to_string() << std::endl;

  auto jnull = new JNull();
  std::cout << "jnull:" << jnull->to_string() << std::endl;

  auto jarray = new JArray();
  jarray->add(new JValue(jstr));
  jarray->add(new JValue(jnum));
  jarray->add(new JValue(jnull));
  auto jobj1 = new JObject();
  jobj1->add("key", new JValue(new JNull()));
  jarray->add(new JValue(jobj1));
  std::cout << "jarray:" << jarray->to_string() << std::endl;

  auto jobject = new JObject();
  jobject->add("jarray", new JValue(jarray));
  jobject->add("jstr", new JValue(new JString("kkkk")));
  std::cout << "jobject:" << jobject->to_string() << std::endl;

  std::cout << "end" << std::endl;
}

}  // namespace ccombinato