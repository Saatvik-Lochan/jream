#include "beam_defs.hpp"
#include "op_arity.hpp"
#include "precompiled.hpp"
#include <algorithm>
#include <cstdint>
#include <glog/logging.h>
#include <stdexcept>

CodeChunk::CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
                     uint32_t label_count)
    : instructions(std::move(instrs)), function_count(function_count),
      label_count(label_count) {

  // allocate label jumps (+1 to count since labels start at 1)
  label_jump_locations = new const uint8_t *[label_count + 1];
  std::fill(label_jump_locations, label_jump_locations + label_count + 1,
            PreCompiled::compile_stub);

  // set all functions as not compiled
  compiled_functions = new const uint8_t *[function_count];
  std::fill(compiled_functions, compiled_functions + function_count, nullptr);

  // compact args for easy asm usage
  const auto len = instructions.size();
  assert(len != 0);

  compacted_arg_p_array = new uint64_t *[len];
  std::fill(compacted_arg_p_array, compacted_arg_p_array + len, nullptr);

  // create function and label table
  func_label_table = new uint64_t[function_count];

  label_table.reserve(label_count + 1);
  label_table.push_back(0); // dummy value so indexing works correctly
  label_func_table.reserve(label_count + 1);
  label_func_table.push_back(0);

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

      current_funcs++;
    }

    if (instr.op_code == LABEL_OP) {
      // the func index is one less than the number of funcs
      label_func_table.push_back(current_funcs - 1);
      label_table.push_back(i);
    }
  }
}

void CodeChunk::set_external_jump_loc(uint64_t index,
                                           CodeChunk *code_chunk_p,
                                           uint64_t label) {
  if (!external_jump_locations) {
    throw std::logic_error(
        "Can't set an external jump location before exports are allocated");
  }

  auto &loc = external_jump_locations[index].ext_id;
  loc.code_chunk = code_chunk_p;
  loc.label = label;
}

void CodeChunk::set_external_jump_loc(uint64_t index, uintptr_t ext_func) {
  if (!external_jump_locations) {
    throw std::logic_error(
        "Can't set an external jump location before exports are allocated");
  }

  external_jump_locations[index].func = ext_func;
}

std::string get_argument_string(Argument arg, const AtomChunk &atom_chunk) {
  std::string tag_name = TagToString(arg.tag) + " = ";
  auto val = arg.arg_raw;
  auto &atoms = atom_chunk.atoms;

  switch (arg.tag) {
  case LITERAL_TAG:
  case LABEL_TAG:
  case INTEGER_TAG:
    return tag_name + std::to_string(val.arg_num);
  case ATOM_TAG:
    return tag_name + atoms[val.arg_num];
  case X_REGISTER_TAG:
    return tag_name + "x" + std::to_string(val.arg_num);
  case Y_REGISTER_TAG:
    return tag_name + "y" + std::to_string(val.arg_num);
  case CHARACTER_TAG:
    return tag_name + static_cast<char>(val.arg_num);
  case EXT_LIST_TAG: {
    const auto &vec = *val.arg_vec_p;

    if (vec.empty()) {
      return tag_name + "[]";
    }

    std::string vec_string = "[ ";

    size_t i = 0;
    for (; i < vec.size() - 1; i++) {
      vec_string += get_argument_string(vec[i], atom_chunk);
      vec_string += ", ";
    }

    vec_string += get_argument_string(vec[i], atom_chunk);
    vec_string += "]";

    return tag_name + vec_string;
  }
  case EXT_ALLOC_LIST_TAG:
  case EXT_LITERAL_TAG:
  case TYPED_REGISTER_TAG:
  case EXT_FPREG_TAG:
  default:
    return tag_name;
  }
}

void AtomChunk::log() {
  LOG(INFO) << "AtomChunk";

  for (size_t i = 1; i < atoms.size(); i++) {
    LOG(INFO) << "    " << i << ":  " << atoms[i];
  }
}

void CodeChunk::log(const AtomChunk &atom_chunk) {
  LOG(INFO) << "CodeChunk";
  LOG(INFO) << "name | op_code/arity";

  for (auto instr : instructions) {
    auto op_code = static_cast<uint32_t>(instr.op_code);
    auto name = op_names[op_code];
    auto arity = op_arities[op_code];

    LOG(INFO) << "";
    LOG(INFO) << std::format("  {} | {}/{} ", name, op_code, arity);

    const auto &args = instr.arguments;

    for (auto arg : args) {
      LOG(INFO) << "    " << get_argument_string(arg, atom_chunk);
    }
  }
}

void LiteralChunk::log() { LOG(INFO) << "Literal Chunk"; }

std::string get_func_id_name(GlobalFunctionIdentifier func_id,
                             const AtomChunk &atom_chunk) {
  auto &atoms = atom_chunk.atoms;
  return std::format("{}:{}/{}", atoms[func_id.module],
                     atoms[func_id.function_name], func_id.arity);
}

void ImportTableChunk::log(const AtomChunk &atom_chunk) {
  LOG(INFO) << "Import Table Chunk";
  for (size_t i = 0; i < imports.size(); i++) {
    LOG(INFO) << "    " << i << ": "
              << get_func_id_name(imports[i], atom_chunk);
  }
}

std::string get_func_id_name(AnonymousFunctionId func_id,
                             const AtomChunk &atom_chunk) {
  auto &atoms = atom_chunk.atoms;
  return std::format("{} - label {}, arity {}, num_free {}",
                     atoms[func_id.function_name], func_id.label, func_id.arity,
                     func_id.num_free);
}

void FunctionTableChunk::log(const AtomChunk &atom_chunk) {
  LOG(INFO) << "Function Table Chunk:";
  for (size_t i = 0; i < functions.size(); i++) {
    LOG(INFO) << "    " << i << ": "
              << get_func_id_name(functions[i], atom_chunk);
  }
}

void BeamSrc::log() {

  atom_chunk.log();
  code_chunk.log(atom_chunk);
  import_table_chunk.log(atom_chunk);
  function_table_chunk.log(atom_chunk);
  literal_chunk.log();
}
