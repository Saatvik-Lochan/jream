#include "beam_defs.hpp"
#include "op_arity.hpp"
#include "precompiled.hpp"
#include "profiler.hpp"
#include <algorithm>
#include <cstdint>
#include <glog/logging.h>
#include <stdexcept>
#include <string>

CodeChunk::CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
                     uint32_t label_count)
    : instructions(std::move(instrs)), function_count(function_count),
      label_count(label_count) {

  PROFILE();

  assert(function_count > 0);
  assert(label_count > 0);

  // allocate label jumps (+1 to count since labels start at 1)
  label_jump_locations = new const uint8_t *[label_count + 1];
  std::fill(label_jump_locations, label_jump_locations + label_count + 1,
            PreCompiled::compile_stub);

  // allocate label offset table
  label_offsets = new size_t[label_count + 1];

  // set all functions as not compiled
  compiled_functions = new const uint8_t *[function_count];
  std::fill(compiled_functions, compiled_functions + function_count, nullptr);

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

      auto label_index = i - 2;
      assert(0 <= label_index);

      auto &label_instr = instructions.at(label_index);

      if (label_instr.op_code != LABEL_OP) {
        throw std::logic_error("No label appears 2 before a func_info op");
      }

      auto label_num = label_instr.arguments.at(0).arg_raw.arg_num;

      assert(current_funcs < function_count);
      func_label_table[current_funcs] = label_num;

      // overwrite (it will be set to previous function)
      label_func_table[label_num] = current_funcs;

      current_funcs++;
    }

    if (instr.op_code == LABEL_OP) {
      // the func index is one less than the number of funcs
      label_func_table.push_back(current_funcs - 1);
      label_table.push_back(i);
    }
  }
}

void CodeChunk::set_external_jump_loc(uint64_t index, CodeChunk *code_chunk_p,
                                      uint64_t label) {
  if (!external_jump_locations) {
    throw std::logic_error(
        "Can't set an external jump location before exports are allocated");
  }

  auto &loc = external_jump_locations[index];
  loc.code_chunk = code_chunk_p;
  loc.label = label;
}

std::string get_argument_string(Argument arg, const AtomChunk &atom_chunk,
                                const LiteralChunk &literal_chunk) {
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
      vec_string += get_argument_string(vec[i], atom_chunk, literal_chunk);
      vec_string += ", ";
    }

    vec_string += get_argument_string(vec[i], atom_chunk, literal_chunk);
    vec_string += "]";

    return tag_name + vec_string;
  }
  case EXT_LITERAL_TAG: {
    auto index = val.arg_num;
    return tag_name + to_string(literal_chunk.literals[index]);
  }
  case EXT_ALLOC_LIST_TAG:
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

void CodeChunk::log(const AtomChunk &atom_chunk,
                    const LiteralChunk &lit_chunk) {
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
      LOG(INFO) << "    " << get_argument_string(arg, atom_chunk, lit_chunk);
    }
  }
}

void LiteralChunk::log() {
  LOG(INFO) << "Literal Chunk";

  for (auto literal : literals) {
    LOG(INFO) << "    " << to_string(literal);
  }
}

std::string get_func_id_name(ExternalFunctionId func_id,
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

std::string get_func_id_name(ExportFunctionId func_id,
                             const AtomChunk &atom_chunk) {
  auto &atoms = atom_chunk.atoms;
  return std::format("{} - label {}, arity {}", atoms[func_id.function_name],
                     func_id.label, func_id.arity);
}

void ExportTableChunk::log(const AtomChunk &atom_chunk) {
  LOG(INFO) << "Export Table Chunk";
  for (size_t i = 0; i < exports.size(); i++) {
    LOG(INFO) << "    " << i << ": "
              << get_func_id_name(exports[i], atom_chunk);
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
  import_table_chunk.log(atom_chunk);
  export_table_chunk.log(atom_chunk);
  function_table_chunk.log(atom_chunk);
  literal_chunk.log();
  code_chunk.log(atom_chunk, literal_chunk);
}

ExportFunctionId BeamSrc::get_external_id(GlobalFunctionId global) {
  assert(module == global.module);

  const auto &map = export_table_chunk.func_to_export;
  auto id_it = map.find(global.function_name + "/" + std::to_string(global.arity));

  if (id_it == map.end()) {
    throw std::logic_error(
        std::format("Could not find function '{}/{}' in module '{}'",
                    global.function_name, global.arity, module));
  }

  return id_it->second;
}
