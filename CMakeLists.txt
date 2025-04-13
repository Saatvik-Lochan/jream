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

set(WITH_GFLAGS OFF)
set(WITH_GTEST OFF)
set(WITH_UNWIND OFF)
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
  src/translation.cpp
  src/bif.cpp
  src/pcb.cpp
  src/exceptions.cpp
  src/execution.cpp
  src/external_term.cpp
  src/asm_callable.cpp
  src/setup_logging.cpp
  src/beam_defs.cpp
  src/precompiled.cpp
  src/generated/instr_code.cpp
)

target_link_libraries(jreamlib PRIVATE
  glog
  zlib
)
target_compile_definitions(jreamlib PRIVATE
  $<$<CONFIG:Debug>:EXEC_LOG=1>
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

set(ASM_OPS_DIR ${PROJECT_SOURCE_DIR}/assembly)
file(GLOB META_ASM_FILES CONFIGURE_DEPENDS "assembly/*_m.S")

# code generation
function(configure_instr_code_gen)
  set(generated_src_file ${SOURCE_CODE_DIR}/generated/instr_code.cpp)
  set(generated_header_file ${INCLUDE_DIR}/generated/instr_code.hpp)

  set(generated_files 
    ${generated_src_file} 
    ${generated_header_file}
  )

  set(generator_script ${SCRIPTS_DIR}/instr_code_gen.py)
  set(callable_fun_file ${INCLUDE_DIR}/asm_callable.hpp)

  add_custom_command(
    OUTPUT ${generated_files}
    COMMAND python ${generator_script}
      ${PROJECT_SOURCE_DIR}
      ${callable_fun_file} 
      ${ASM_OPS_DIR}
      ${generated_src_file}
      ${generated_header_file}
    DEPENDS
      ${SCRIPTS}
      ${META_ASM_FILES}
    COMMENT "Generating ${generated_files}"
  )
  add_custom_target(instr_code_gen
    DEPENDS ${generated_file}
  )
  add_dependencies(jreamlib instr_code_gen)
endfunction()
configure_instr_code_gen()

function(configure_shared_variable_gen)
  set(generated_file ${INCLUDE_DIR}/generated/shared_variables.hpp)
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
