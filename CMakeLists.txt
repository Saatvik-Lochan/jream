cmake_minimum_required(VERSION 3.14)
project(jream LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic")

# packages
find_package(glog REQUIRED)
find_package(ZLIB REQUIRED)

# main library
add_library(jreamlib 
  src/beamparser.cpp
  src/exceptions.cpp
  src/execution.cpp
  src/external_term.cpp
  src/generated/instr_code.cpp
)

target_link_libraries(jreamlib PRIVATE
  glog::glog
  ZLIB::ZLIB
)
target_include_directories(jreamlib
  PRIVATE
  ${PROJECT_SOURCE_DIR}/include
)

# main executable
add_executable(jream src/main.cpp)
target_link_libraries(jream PRIVATE
  jreamlib
)

enable_testing()
add_subdirectory(tests)
add_test(NAME JreamTest COMMAND jream_tests)
