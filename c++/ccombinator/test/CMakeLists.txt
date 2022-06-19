enable_testing()

include_directories(
    ${SRC_DIR}
    include(GoogleTest)
)

add_executable(parser-test test.cc)
target_sources(parser-test
PRIVATE
    result-test.cc
    json/json-parser.cc
    json/json-test.cc
    json/json.cc
)
target_link_libraries(parser-test
PRIVATE
    gtest
    pthread
)
gtest_discover_tests(parser-test
)
