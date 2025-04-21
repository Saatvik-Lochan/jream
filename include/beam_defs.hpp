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

struct ExternalFunctionId {
  // indexes to the appropriate atom in the atom table
  uint32_t module;
  uint32_t function_name;

  uint32_t arity;

  bool operator==(const ExternalFunctionId &other) const = default;
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

struct ExportFunctionId {
  uint32_t function_name;
  uint32_t arity;
  uint32_t label;

  bool operator==(const ExportFunctionId &other) const = default;
};

struct GlobalFunctionId {
  std::string module;
  std::string function_name;
  uint32_t arity;

  bool operator==(const GlobalFunctionId &other) const = default;
};

template <> struct std::hash<ExternalFunctionId> {
  size_t operator()(const ExternalFunctionId &id) const {
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
  std::unordered_map<std::string, uint64_t> atom_index;

  AtomChunk(std::vector<std::string> atoms_) : atoms(std::move(atoms_)) {
    uint64_t count = 0;
    for (auto &atom : atoms) {
      atom_index[atom] = count++;
    }
  }

  void log();
};

struct FunctionTableChunk {
  std::vector<AnonymousFunctionId> functions;

  FunctionTableChunk(std::vector<AnonymousFunctionId> functions)
      : functions(std::move(functions)) {}

  void log(const AtomChunk &atom_chunk);
};

struct ImportTableChunk {
  std::vector<ExternalFunctionId> imports;

  ImportTableChunk(std::vector<ExternalFunctionId> imports)
      : imports(std::move(imports)) {}

  void log(const AtomChunk &atom_chunk);
};

struct ExportTableChunk {
  std::vector<ExportFunctionId> exports;
  std::unordered_map<std::string, ExportFunctionId> func_to_export;

  ExportTableChunk(std::vector<ExportFunctionId> exports)
      : exports(std::move(exports)) {}

  void log(const AtomChunk &atom_chunk);
};

struct LiteralChunk {
  std::vector<ErlTerm> literals;

  LiteralChunk(std::vector<ErlTerm> literals) : literals(std::move(literals)) {}

  void log();
};

using FunctionLabelTable = uint64_t *;
using LabelTable = std::vector<uint64_t>;
using LabelFunctionTable = std::vector<uint64_t>;
using LabelOffsetTable = size_t *;

struct CodeChunk;

// we're making some assumptions on the layout of memory here
struct EntryPoint {
  CodeChunk *code_chunk;
  uint64_t label;
};

static_assert(sizeof(EntryPoint) == 16,
              "ExtJump must be 16 bytes so that we can index into it");

struct CodeChunk {
  std::vector<Instruction> instructions;
  uint32_t function_count;
  uint32_t label_count;

  FunctionLabelTable func_label_table;
  LabelFunctionTable label_func_table;
  LabelTable label_table;
  LabelOffsetTable label_offsets;

  // reserve space for 256, we cannot have this pointer move/reallocate!
  // 256 * 8 = 2048 which is the maximum addressable range of a 12 bit
  // immediate
  uint64_t *compacted_arg_p_array[256];
  uint64_t compacted_arr_next_free = 0;

  const uint8_t *volatile *label_jump_locations;
  const uint8_t **compiled_functions;

  volatile EntryPoint *external_jump_locations = nullptr;

  AtomChunk *atom_chunk;
  ImportTableChunk *import_table_chunk;
  FunctionTableChunk *function_table_chunk;
  LiteralChunk *literal_chunk;

  CodeChunk(std::vector<Instruction> instrs, uint32_t function_count,
            uint32_t label_count);

  void set_external_jump_loc(uint64_t index, CodeChunk *, uint64_t label);

  void log(const AtomChunk &atom_chunk, const LiteralChunk &literal_chunk);
};

struct BeamSrc {

  std::string module;

  AtomChunk atom_chunk;
  CodeChunk code_chunk;
  LiteralChunk literal_chunk;
  ImportTableChunk import_table_chunk;
  ExportTableChunk export_table_chunk;
  FunctionTableChunk function_table_chunk;

  BeamSrc(const BeamSrc &) = delete;            
  BeamSrc &operator=(const BeamSrc &) = delete; 
  BeamSrc(BeamSrc &&) = delete;                  
  BeamSrc &operator=(BeamSrc &&) = delete;       

  BeamSrc(AtomChunk atom_chunk_, CodeChunk code_chunk_,
          LiteralChunk literal_chunk_, ImportTableChunk import_table_chunk_,
          ExportTableChunk export_table_chunk_,
          FunctionTableChunk function_table_chunk_)
      : atom_chunk(std::move(atom_chunk_)), code_chunk(std::move(code_chunk_)),
        literal_chunk(std::move(literal_chunk_)),
        import_table_chunk(std::move(import_table_chunk_)),
        export_table_chunk(std::move(export_table_chunk_)),
        function_table_chunk(std::move(function_table_chunk_)) {

    // set module
    assert(atom_chunk.atoms.size() > 1);
    module = atom_chunk.atoms[1];

    // link BeamSrc
    code_chunk.import_table_chunk = &import_table_chunk;
    code_chunk.function_table_chunk = &function_table_chunk;
    code_chunk.atom_chunk = &atom_chunk;
    code_chunk.literal_chunk = &literal_chunk;

    code_chunk.external_jump_locations =
        new EntryPoint[import_table_chunk.imports.size()];

    // link Export chunk
    for (auto exp : export_table_chunk.exports) {
      auto func_name = atom_chunk.atoms[exp.function_name];
      export_table_chunk.func_to_export[func_name] = exp;
    }
  }

  void log();
  ExportFunctionId get_external_id(GlobalFunctionId id);
};

#endif
