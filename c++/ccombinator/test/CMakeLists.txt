enable_testing()

include_directories(
    ${SRC_DIR}
    include(GoogleTest)
)

add_executable(parser-test test.cc)
target_sources(parser-test
PRIVATE
    result-test.cc
)
target_link_libraries(parser-test
PRIVATE
    gtest
    pthread
)
gtest_discover_tests(parser-test
)
