#include "external_term.hpp"
#include "op_arity.hpp"

#include <cassert>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <vector>

#ifndef BEAM_DEFS
#define BEAM_DEFS

// data structures
enum Tag {
  LITERAL_TAG,
  INTEGER_TAG,
  ATOM_TAG,
  X_REGISTER_TAG,
  Y_REGISTER_TAG,
  LABEL_TAG,
  CHARACTER_TAG,
  EXT_LIST_TAG,
  EXT_FPREG_TAG,
  EXT_ALLOC_LIST_TAG,
  EXT_LITERAL_TAG,
  TYPED_REGISTER_TAG
};

std::string TagToString(Tag tag);

struct TypedRegister {
  Tag reg;
  uint64_t reg_num;
  uint64_t index;
};

struct AllocList {
  uint64_t words;
  uint64_t floats;
  uint64_t funs;
};

struct Argument {
  Tag tag;
  union {
    std::vector<Argument> *arg_vec_p; // when EXT_LIST
    AllocList *alloc_list;            // when EXT_ALLOC_LIST
    uint64_t arg_num;                 // otherwise
  } arg_raw;
};

struct Instruction {
  OpCode op_code;
  std::vector<Argument> arguments;
};

struct FunctionIdentifier {
  uint64_t module; // indexes to the appropriate atom in the atom table
  uint64_t function_name;
  uint64_t arity;

  bool operator==(const FunctionIdentifier &other) const = default;
};

template <> struct std::hash<FunctionIdentifier> {
  size_t operator()(const FunctionIdentifier &id) const {
    size_t h1 = std::hash<uint64_t>{}(id.module);
    size_t h2 = std::hash<uint64_t>{}(id.function_name);
    size_t h3 = std::hash<uint64_t>{}(id.arity);

    return h1 ^ (h2 << 1) ^ (h3 << 2);
  }
};

struct CodeSection {
  uint64_t start;
  uint64_t end;

  bool operator==(const CodeSection &other) const = default;
};

template <> struct std::hash<CodeSection> {
  size_t operator()(const CodeSection &cs) const {
    return std::hash<size_t>{}(cs.start) ^ std::hash<size_t>{}(cs.end);
  }
};

using FunctionLabelTable = uint64_t *;
using LabelTable = std::vector<uint64_t>;
using LabelFunctionTable = std::unordered_map<uint64_t, uint64_t>;

struct CodeChunk {
  std::vector<Instruction> instructions;
  uint32_t function_count;
  uint32_t label_count;

  FunctionLabelTable func_label_table;
  LabelFunctionTable label_func_table;
  LabelTable label_table;

  uint64_t **compacted_arg_p_array;
  const uint8_t *volatile *compiled_code_lookup;

  CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
            uint32_t label_count);
};

struct AtomChunk {
  std::vector<std::string> atoms;

  AtomChunk(std::vector<std::string> atoms) : atoms(std::move(atoms)) {}
};

struct LiteralChunk {
  std::vector<ErlTerm> literals;

  LiteralChunk(std::vector<ErlTerm> literals) : literals(std::move(literals)) {}
};

struct BeamFile {
  AtomChunk atom_chunk;
  CodeChunk code_chunk;
  LiteralChunk literal_chunk;

  BeamFile(AtomChunk atom_chunk, CodeChunk code_chunk,
           LiteralChunk literal_chunk)
      : atom_chunk(std::move(atom_chunk)), code_chunk(std::move(code_chunk)),
        literal_chunk(literal_chunk) {}
};

#endif
