#include "asm_callable.hpp"
#include "execution.hpp"
#include <iostream>

void print_int(uint64_t a) { std::cout << a << std::endl; }

uint8_t *compile_function(CodeChunk *code_chunk, uint64_t func_index) {
  return compile_erlang_func(*code_chunk, func_index);
}
