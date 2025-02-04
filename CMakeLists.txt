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
)

set(HEADERS
    exceptions.h
    op_arity.h
    beam_defs.h
    external_term.h
)

# Define the executable
add_executable(beamparser ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(beamparser PRIVATE z glog) # zlib and Google Logging

# Debug build target
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build" FORCE)
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
