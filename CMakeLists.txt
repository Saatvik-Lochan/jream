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

# install gtest
include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest  
  GIT_TAG v1.15.2  
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

include(GoogleTest)

add_executable(jream 
  src/beamparser.cpp
  src/exceptions.cpp
  src/execution.cpp
  src/external_term.cpp
  src/generated/instr_code.cpp
)

target_link_libraries(jream PRIVATE
  glog::glog
  ZLIB::ZLIB
)
target_include_directories(jream
  PRIVATE
  ${PROJECT_SOURCE_DIR}/include
)
