#include "external_term.h"
#include "op_arity.h"

#include <cstdint>
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
  OpCode opCode;
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

using FunctionTable = std::unordered_map<FunctionIdentifier, Instruction *>;
using LabelTable = std::vector<Instruction *>;

struct CodeChunk {
  std::vector<Instruction> instructions;
  uint32_t function_count;
  uint32_t label_count;

  FunctionTable function_table;
  LabelTable label_table;

  CodeChunk(std::vector<Instruction> instructions, uint32_t function_count,
            uint32_t label_count, FunctionTable function_table,
            LabelTable label_table)
      : instructions(std::move(instructions)), function_count(function_count),
        label_count(label_count), function_table(std::move(function_table)),
        label_table(std::move(label_table)) {}
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
