add_library(ccombinator INTERFACE)

target_sources(ccombinator
  PUBLIC
    ccombinator.h
)

add_executable(main main.cc)
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
target_link_libraries(main PRIVATE Threads::Threads)