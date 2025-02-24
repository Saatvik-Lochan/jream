# add gtest
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest  
  GIT_TAG v1.15.2  
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

include(GoogleTest)

# add testing executable
add_executable(jream_tests
  test_main.cpp
)
target_link_libraries(jream_tests PRIVATE
  jreamlib
  gtest_main
  ${STATIC_GLOG_PATH}
)
