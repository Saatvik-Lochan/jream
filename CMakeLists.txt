cmake_minimum_required(VERSION 3.14)
project(jream CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")

# Source files
set(SOURCES
    beamparser.cpp
    exceptions.cpp
    external_term.cpp
    execution.cpp
    assembly/instr_code.cpp
)

set(HEADERS
    exceptions.h
    op_arity.h
    beam_defs.h
    external_term.h
    assembly/instr_code.h
)

include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest  
  GIT_TAG v1.15.2  
)
FetchContent_MakeAvailable(googletest)

set(MAIN_LIBRARIES
   
)

add_executable(beamparser ${SOURCES} ${HEADERS})
target_link_libraries(beamparser PRIVATE z glog) # zlib and Google Logging

# testing
set(TEST_SOURCES
  tests/test.cpp
)
add_executable(tests ${TEST_SOURCES} ${SOURCES} ${HEADERS})
target_link_libraries(tests PRIVATE gtest_main glog)

# add tests
include(GoogleTest)
gtest_discover_tests(tests)
