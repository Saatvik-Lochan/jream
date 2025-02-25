cmake_minimum_required(VERSION 3.14)
project(jream LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic")

# Want to support cross compilation, so must prefer static libs
# and building from source with FetchContent
include(FetchContent)
FetchContent_Declare(
  glog
  GIT_REPOSITORY https://github.com/google/glog.git
  GIT_TAG v0.7.1
)
FetchContent_MakeAvailable(glog)

FetchContent_Declare(
  zlib
  GIT_REPOSITORY https://github.com/madler/zlib  
  GIT_TAG v1.3.1
)
FetchContent_MakeAvailable(zlib)

# main library
add_library(jreamlib 
  src/beamparser.cpp
  src/exceptions.cpp
  src/execution.cpp
  src/external_term.cpp
  src/asm_callable.cpp
  src/setup_logging.cpp
  src/generated/instr_code.cpp
)

# target_compile_definitions (jreamlib PUBLIC GLOG_USE_GLOG_EXPORT)
target_link_libraries(jreamlib PRIVATE
  glog
  zlib
)
target_include_directories(jreamlib PRIVATE
  ${PROJECT_SOURCE_DIR}/include
)

# convenience definitions
set(SOURCE_CODE_DIR ${PROJECT_SOURCE_DIR}/src)
set(SCRIPTS_DIR ${PROJECT_SOURCE_DIR}/scripts)
set(INCLUDE_DIR ${PROJECT_SOURCE_DIR}/include)
set(SCRIPTS
  ${SCRIPTS_DIR}/get_binary.sh
  ${SCRIPTS_DIR}/riscv_from_meta.sh

  ${SCRIPTS_DIR}/shared_variables.py
  ${SCRIPTS_DIR}/shared_variables_gen.py
  ${SCRIPTS_DIR}/meta_assembly_compile.py
  ${SCRIPTS_DIR}/instr_code_gen.py
)
set(ASM_OPS_DIR ${PROJECT_SOURCE_DIR}/assembly/ops)

set(META_ASM_FILES
  ${ASM_OPS_DIR}/call_m.S
  ${ASM_OPS_DIR}/allocate_m.S
)

# code generation
function(configure_instr_code_gen)
  set(generated_file ${SOURCE_CODE_DIR}/generated/instr_code.cpp)
  set(generator_script ${SCRIPTS_DIR}/instr_code_gen.py)
  set(callable_fun_file ${INCLUDE_DIR}/asm_callable.h)

  add_custom_command(
    OUTPUT ${generated_file}
    COMMAND python ${generator_script}
      ${ASM_OPS_DIR}
      ${generated_file}
      ${callable_fun_file} 
    DEPENDS
      ${SCRIPTS}
      ${META_ASM_FILES}
    COMMENT "Generating ${generated_file}"
  )
  add_custom_target(instr_code_gen
    DEPENDS ${generated_file}
  )
  add_dependencies(jreamlib instr_code_gen)
endfunction()
configure_instr_code_gen()

function(configure_shared_variable_gen)
  set(generated_file ${INCLUDE_DIR}/generated/shared_variables.h)
  set(generator_script ${SCRIPTS_DIR}/shared_variables_gen.py)

  add_custom_command(
    OUTPUT ${generated_file}
    COMMAND python ${generator_script} ${generated_file}
    DEPENDS ${generator_script} ${SCRIPTS_DIR}/shared_variables.py
    COMMENT "Generating ${generated_file}"
  )
  add_custom_target(shared_variables_gen
    DEPENDS ${generated_file})
  add_dependencies(jreamlib shared_variables_gen)
endfunction()
configure_shared_variable_gen()

# main executable
add_executable(trial src/trial_jit.cpp)
target_link_libraries(trial PRIVATE
  jreamlib
  glog
)

enable_testing()
add_subdirectory(tests)
add_test(NAME JreamTest COMMAND jream_tests)
