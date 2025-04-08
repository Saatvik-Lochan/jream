#ifndef BEAM_DEFS
#define BEAM_DEFS

#include "external_term.hpp"
#include "op_arity.hpp"

#include <cassert>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <vector>

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
    const std::vector<Argument> *arg_vec_p; // when EXT_LIST
    AllocList *alloc_list;                  // when EXT_ALLOC_LIST
    uint64_t arg_num;                       // otherwise
  } arg_raw;
};

struct Instruction {
  OpCode op_code;
  std::vector<Argument> arguments;
};

struct GlobalFunctionIdentifier {
  uint32_t module; // indexes to the appropriate atom in the atom table
  uint32_t function_name;
  uint32_t arity;

  bool operator==(const GlobalFunctionIdentifier &other) const = default;
};

struct AnonymousFunctionId {
  uint32_t function_name;
  uint32_t arity;
  uint32_t label;
  uint32_t index;
  uint32_t num_free;
  uint32_t old_uniq;

  bool operator==(const AnonymousFunctionId &other) const = default;
};

template <> struct std::hash<GlobalFunctionIdentifier> {
  size_t operator()(const GlobalFunctionIdentifier &id) const {
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

struct AtomChunk {
  std::vector<std::string> atoms;

  AtomChunk(std::vector<std::string> atoms) : atoms(std::move(atoms)) {}

  void log();
};

struct FunctionTableChunk {
  std::vector<AnonymousFunctionId> functions;

  FunctionTableChunk(std::vector<AnonymousFunctionId> functions)
      : functions(std::move(functions)) {}

  void log(const AtomChunk &atom_chunk);
};

struct ImportTableChunk {
  std::vector<GlobalFunctionIdentifier> imports;

  ImportTableChunk(std::vector<GlobalFunctionIdentifier> imports)
      : imports(std::move(imports)) {}

  void log(const AtomChunk &atom_chunk);
};

using FunctionLabelTable = uint64_t *;
using LabelTable = std::vector<uint64_t>;
using LabelFunctionTable = std::vector<uint64_t>;
using LabelOffsetTable = std::unordered_map<uint64_t, size_t>;

struct CodeChunk;

// we're making some assumptions on the layout of memory here
union ExtJump {
  uintptr_t func;
  struct {
    CodeChunk *code_chunk;
    uint64_t label;
  } ext_id;
};

static_assert(sizeof(ExtJump) == 16,
              "ExtJump must be 16 bytes so that we can index into it");

struct CodeChunk {
  std::vector<Instruction> instructions;
  uint32_t function_count;
  uint32_t label_count;

  FunctionLabelTable func_label_table;
  LabelFunctionTable label_func_table;
  LabelTable label_table;
  LabelOffsetTable label_offsets;

  uint64_t **compacted_arg_p_array;
  const uint8_t *volatile *label_jump_locations;
  const uint8_t **compiled_functions;

  volatile ExtJump *external_jump_locations = nullptr;

  AtomChunk *atom_chunk;
  ImportTableChunk *import_table_chunk;
  FunctionTableChunk *function_table_chunk;

  CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
            uint32_t label_count);

  void set_external_jump_loc(uint64_t index, CodeChunk *, uint64_t label);
  void set_external_jump_loc(uint64_t index, uintptr_t);

  void log(const AtomChunk &atom_chunk);
};

struct LiteralChunk {
  std::vector<ErlTerm> literals;

  LiteralChunk(std::vector<ErlTerm> literals) : literals(std::move(literals)) {}

  void log();
};

struct BeamSrc {
  AtomChunk atom_chunk;
  CodeChunk code_chunk;
  LiteralChunk literal_chunk;
  ImportTableChunk import_table_chunk;
  FunctionTableChunk function_table_chunk;

  BeamSrc(AtomChunk atom_chunk, CodeChunk code_chunk,
          LiteralChunk literal_chunk, ImportTableChunk import_table_chunk,
          FunctionTableChunk function_table_chunk)
      : atom_chunk(std::move(atom_chunk)), code_chunk(std::move(code_chunk)),
        literal_chunk(std::move(literal_chunk)),
        import_table_chunk(std::move(import_table_chunk)),
        function_table_chunk(std::move(function_table_chunk)) {

    this->code_chunk.import_table_chunk = &this->import_table_chunk;
    this->code_chunk.function_table_chunk = &this->function_table_chunk;
    this->code_chunk.atom_chunk = &this->atom_chunk;

    this->code_chunk.external_jump_locations = new ExtJump[import_table_chunk.imports.size()];
  }

  void log();
};

#endif
