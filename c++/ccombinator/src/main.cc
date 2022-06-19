#include <unistd.h>

#include <cstring>
#include <iostream>
#include <thread>

int main(void) {
  std::cout << "start" << std::endl;
  // std::string original_str;
  // original_str.reserve(32);

  char original_str[29 + 1] = {};

  auto updater = [&original_str]() {
    bool a = true;
    while (true) {
      // original_str = std::string("aaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
      std::string temp;
      if (a) {
        temp = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
      } else {
        temp = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
      }
      a = !a;
      strcpy(original_str, temp.c_str());
    }
  };

  auto referrer = [&original_str]() {
    while (true) {
      // std::string copy_str;
      // copy_str.reserve(32);
      // copy_str = original_str;
      // std::cout << copy_str << std::endl;

      // char result[29 + 1] = {};
      // original_str.copy(result, 29);
      // std::string copy_str(result);
      // std::cout << copy_str << std::endl;

      char* ref = original_str;
      std::cout << ref << std::endl;

      usleep(1000);
    }
  };

  std::thread t1(updater);
  std::thread t2(referrer);
  t1.join();
  t2.join();

  std::cout << "end" << std::endl;
}