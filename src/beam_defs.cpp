#include "beam_defs.hpp"
#include "precompiled.hpp"
#include <algorithm>
#include <glog/logging.h>

CodeChunk::CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
                     uint32_t label_count)
    : instructions(std::move(instrs)), function_count(function_count),
      label_count(label_count) {

  // allocate function table
  compiled_code_lookup = new const uint8_t *[function_count];
  std::fill(compiled_code_lookup, compiled_code_lookup + function_count,
            PreCompiled::compile_stub);

  // compact args for easy asm usage
  const auto len = instructions.size();
  assert(len != 0);

  compacted_arg_p_array = new uint64_t *[len];
  std::fill(compacted_arg_p_array, compacted_arg_p_array + len, nullptr);

  // create function and label table
  func_label_table = new uint64_t[function_count];
  label_table.reserve(label_count + 1);
  label_table.push_back(0); // dummy value so indexing works correctly

  uint64_t current_funcs = 0;

  for (size_t i = 0; i < instructions.size(); i++) {
    auto &instr = instructions[i];

    if (instr.op_code == FUNC_INFO_OP) {
      const auto &args = instr.arguments;

      [[maybe_unused]]
      auto module_atom_index = args[0];
      assert(module_atom_index.tag == ATOM_TAG);

      [[maybe_unused]]
      auto function_name_atom_index = args[1];
      assert(function_name_atom_index.tag == ATOM_TAG);

      [[maybe_unused]]
      auto arity = args[2];
      assert(arity.tag == LITERAL_TAG);

      auto &label_instr = instructions.at(i + 1);
      if (label_instr.op_code != LABEL_OP) {
        throw std::logic_error("No label appears after a func_info op");
      }

      auto label_num = label_instr.arguments.at(0).arg_raw.arg_num;

      assert(current_funcs < function_count);
      func_label_table[current_funcs] = label_num;
      label_func_table[label_num] = current_funcs;

      current_funcs++;

    }

    if (instr.op_code == LABEL_OP) {
      label_table.push_back(i);
    }
  }
}
