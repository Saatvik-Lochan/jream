#ifndef TRANSLATION
#define TRANSLATION

#include "beam_defs.hpp"
#include <cstdint>

uint8_t *move_code_to_memory(const std::span<const uint8_t> &code);

uint8_t *compile_erlang_func(CodeChunk &code_chunk, uint64_t func_index);

#endif 
