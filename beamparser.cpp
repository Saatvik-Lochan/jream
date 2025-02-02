#define GLOG_USE_GLOG_EXPORT
#include <glog/logging.h>

#include "exceptions.h"
#include "external_term.h"
#include "int_from_bytes.h"
#include "op_arity.h"
#include "beam_defs.h"

#include <cassert>
#include <cstdint>
#include <format>
#include <fstream>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <stdexcept>
#include <string>
#include <sys/types.h>
#include <utility>
#include <vector>
#include <zlib.h>

// Parsing Utilities
std::string read_string(std::ifstream &stream, size_t length) {
  std::string buffer(length, '\0');
  stream.read(&buffer[0], length);
  return buffer;
}

uint8_t read_byte(std::ifstream &stream) {
  uint8_t out;
  stream.read(reinterpret_cast<char *>(&out), 1);
  return out;
}

std::uint32_t read_big_endian(std::ifstream &stream) {
  uint8_t buffer[4];
  stream.read(reinterpret_cast<char *>(buffer), 4);
  return big_endian_from_bytes<uint32_t>(buffer);
}


constexpr std::string TagToString(Tag tag) {
  switch (tag) {
  case LITERAL_TAG:
    return "Literal";
  case INTEGER_TAG:
    return "Integer";
  case ATOM_TAG:
    return "Atom";
  case X_REGISTER_TAG:
    return "X Register";
  case Y_REGISTER_TAG:
    return "Y Register";
  case LABEL_TAG:
    return "Label";
  case CHARACTER_TAG:
    return "Character";
  case EXT_LIST_TAG:
    return "Extended List";
  case EXT_FPREG_TAG:
    return "Extended Floating Point Register";
  case EXT_ALLOC_LIST_TAG:
    return "Extended Alloc List";
  case EXT_LITERAL_TAG:
    return "Extended Literal";
  case TYPED_REGISTER_TAG:
    return "Typed Register";
  default:
    LOG(FATAL) << "unknown tag";
  }
}

// parsing
AtomChunk parse_atom_chunk(std::ifstream &stream) {
  uint32_t num_atoms = read_big_endian(stream);
  std::vector<std::string> atoms;

  LOG(INFO) << "Atoms:" << std::endl;

  for (uint32_t i = 0; i < num_atoms; i++) {
    uint8_t atom_length = read_byte(stream);
    std::string atom_name = read_string(stream, atom_length);

    LOG(INFO) << "\t" << atom_name << std::endl;

    atoms.push_back(atom_name);
  }

  return AtomChunk(std::move(atoms));
}

enum Tag parse_tag(uint8_t tag_byte) {
  const uint8_t mask = 0b111;

  switch (tag_byte & mask) {
  case 0b000:
    return LITERAL_TAG;
  case 0b001:
    return INTEGER_TAG;
  case 0b010:
    return ATOM_TAG;
  case 0b011:
    return X_REGISTER_TAG;
  case 0b100:
    return Y_REGISTER_TAG;
  case 0b101:
    return LABEL_TAG;
  case 0b110:
    return CHARACTER_TAG;
  case 0b111: {
    const uint8_t ext_bits = tag_byte >> 4;

    switch (ext_bits) {
    case 0b0001:
      return EXT_LIST_TAG;
    case 0b0010:
      return EXT_FPREG_TAG;
    case 0b0011:
      return EXT_ALLOC_LIST_TAG;
    case 0b0100:
      return EXT_LITERAL_TAG;
    case 0b101:
      return TYPED_REGISTER_TAG;
    default:
      LOG(FATAL) << std::format("No such tag: {:08b}", tag_byte);
    }
  }
  default:
    LOG(FATAL) << "Cannot reach here";
  }
}

// TODO maybe support bigger numbers, I can't imagine when you would get
// something this big though
uint64_t parse_argument_number(std::ifstream &stream, uint8_t tag_byte) {

  if (tag_byte >> 3 == 0b11111) {

    // auto next_byte = read_byte(stream);
    // auto tag = parse_tag(next_byte);

    // assert(tag == LITERAL);

    // auto embedded_size = parse_argument_number(stream, next_byte);

    // std::vector<uint8_t> value(embedded_size);
    // stream.read(reinterpret_cast<char *>(value.data()), embedded_size);

    // return value;

    throw NotImplementedException("Have not implemented parsing of integers "
                                  "greater than those which fit in uint64_t");
  }

  if (tag_byte & (1 << 3)) {
    if (tag_byte & (1 << 4)) {

      const uint8_t num_following_bytes = (tag_byte >> 5) + 2;

      uint64_t value = 0;

      // TODO check endianess!
      for (int i = 0; i < num_following_bytes; i++) {
        uint8_t next_byte = read_byte(stream);
        value |= static_cast<uint64_t>(next_byte) << (i * 8);
      }

      return value;

    } else {

      // TODO check bit ordering
      uint64_t value =
          static_cast<uint64_t>(tag_byte & 0b11100000) << 3 & read_byte(stream);
      return value;
    }
  } else {

    const uint64_t value = tag_byte >> 4;
    return value;
  }
}

// TODO incomplete?
uint64_t parse_extended_literal(std::ifstream &stream) {
  auto next_byte = read_byte(stream);
  auto tag = parse_tag(next_byte);
  LOG(INFO) << TagToString(tag) << std::endl;

  assert(tag == LITERAL_TAG);

  auto embedded_size = parse_argument_number(stream, next_byte);
  return embedded_size;
}

Argument parse_argument(std::ifstream &stream);

std::vector<Argument> *parse_extended_list(std::ifstream &stream) {
  auto next_byte = read_byte(stream);
  auto tag = parse_tag(next_byte);

  assert(tag == LITERAL_TAG);

  auto list_size = parse_argument_number(stream, tag);
  // assumed to last the lifetime of the program, so no delete/cleanup
  auto arg_vec_p = new std::vector<Argument>(list_size);

  for (uint64_t i = 0; i < list_size; i++) {
    arg_vec_p->push_back(parse_argument(stream));
  }

  return arg_vec_p;
}

AllocList *parse_alloc_list(std::ifstream &stream) {
  auto length_byte = read_byte(stream);
  auto tag = parse_tag(length_byte);

  assert(tag == LITERAL_TAG);

  auto list_length = parse_argument_number(stream, length_byte);

  AllocList *out_list = new AllocList{.words = 0, .floats = 0, .funs = 0};

  for (uint64_t i = 0; i < list_length; i++) {
    auto type_byte = read_byte(stream);
    tag = parse_tag(length_byte);

    assert(tag == LITERAL_TAG);

    auto type = parse_argument_number(stream, type_byte);

    auto num_byte = read_byte(stream);
    tag = parse_tag(num_byte);

    assert(tag == LITERAL_TAG);

    auto num = parse_argument_number(stream, num_byte);

    switch (type) {
    case 0: {
      out_list->words = num;
      break;
    }
    case 1: {
      out_list->floats = num;
      break;
    }
    case 2: {
      out_list->funs = num;
      break;
    }
    default: {
      LOG(FATAL) << "Unknown type in alloc list " + std::to_string(type);
    }
    }
  }

  return out_list;
}

TypedRegister parse_typed_register(std::ifstream &stream) {
  const auto register_byte = read_byte(stream);
  const auto register_tag = parse_tag(register_byte);

  assert(register_tag == X_REGISTER_TAG || register_tag == Y_REGISTER_TAG);

  auto register_num = parse_argument_number(stream, register_byte);

  const auto index_byte = read_byte(stream);
  const auto index_tag = parse_tag(index_byte);

  assert(index_tag == LITERAL_TAG);
  auto index_num = parse_argument_number(stream, index_byte);

  return TypedRegister{register_tag, register_num, index_num};
}

Argument parse_argument(std::ifstream &stream) {
  uint8_t tag_byte = read_byte(stream);
  enum Tag tag = parse_tag(tag_byte);

  LOG(INFO) << "\t" << TagToString(tag);

  switch (tag) {
  case LITERAL_TAG:
  case INTEGER_TAG:
  case ATOM_TAG:
  case X_REGISTER_TAG:
  case Y_REGISTER_TAG:
  case LABEL_TAG:
  case CHARACTER_TAG: {
    auto index = parse_argument_number(stream, tag_byte);
    return Argument{tag, {.arg_num = index}};
  }
  case EXT_LITERAL_TAG: {
    [[maybe_unused]]
    auto index = parse_extended_literal(stream);
    throw NotImplementedException("Must fix parse extended literal first");
  }
  case TYPED_REGISTER_TAG: {
    auto typed_register = parse_typed_register(stream);
    // TODO use the type which is indexed by typed_register.index
    return Argument{typed_register.reg, {.arg_num = typed_register.reg_num}};
  }
  case EXT_LIST_TAG: {
    auto result = parse_extended_list(stream);
    return Argument{tag, {.arg_vec_p = result}};
  }
  case EXT_ALLOC_LIST_TAG: {
    auto result = parse_alloc_list(stream);
    return Argument{tag, {.alloc_list = result}};
  }
  case EXT_FPREG_TAG: {
    std::string error_msg =
        std::format("Tag '{}' not implemented yet", TagToString(tag));
    throw NotImplementedException(error_msg.c_str());
  }
  default:
    LOG(FATAL) << "invalid tag";
  }
}

CodeChunk parse_code_chunk(std::ifstream &stream, std::streampos chunk_end) {

  // TODO use these for optimising allocations
  const uint32_t sub_size = read_big_endian(stream);
  LOG(INFO) << "sub_size: " << sub_size << std::endl;

  auto chunk_start = stream.tellg();

  const uint32_t _instruction_set_version [[maybe_unused]] =
      read_big_endian(stream);
  const uint32_t _op_code_max [[maybe_unused]] = read_big_endian(stream);

  const uint32_t label_count = read_big_endian(stream);
  LOG(INFO) << std::format("label count: {}", label_count) << std::endl;

  const uint32_t function_count = read_big_endian(stream);

  // skip till subsize amount forward
  stream.seekg(chunk_start + static_cast<std::streamoff>(sub_size));

  // read code
  std::vector<Instruction> instructions;

  while (stream.tellg() < chunk_end) {
    uint8_t op_code = read_byte(stream);
    uint8_t arity = op_arities[op_code];

    LOG(WARNING) << std::format("{}, op_code: {}, arity: {} ",
                                op_names[op_code], op_code, arity);

    std::vector<Argument> args(arity);

    for (int i = 0; i < arity; i++) {
      args[i] = parse_argument(stream);
    }

    instructions.push_back(
        Instruction(static_cast<OpCode>(op_code), std::move(args)));
  }

  // create function and label table
  FunctionTable function_table;
  LabelTable label_table;
  label_table.reserve(label_count + 1);

  label_table.push_back(nullptr); // dummy value so indexing works correctly

  for (auto instr : instructions) {
    if (instr.opCode == FUNC_INFO_OP) {

      const auto &args = instr.arguments;

      auto module_atom_index = args[0];
      assert(module_atom_index.tag == ATOM_TAG);

      auto function_name_atom_index = args[1];
      assert(function_name_atom_index.tag == ATOM_TAG);

      auto arity = args[2];
      assert(arity.tag == LITERAL_TAG);

      auto func_id = FunctionIdentifier(
          module_atom_index.arg_raw.arg_num,
          function_name_atom_index.arg_raw.arg_num, arity.arg_raw.arg_num);

      function_table[func_id] = label_table[label_table.size() - 1];
    }

    if (instr.opCode == LABEL_OP) {
      label_table.push_back(&instr);
    }
  }

  return CodeChunk(std::move(instructions), function_count, label_count,
                   function_table, label_table);
}

LiteralChunk parse_literal_chunk(std::ifstream &stream,
                                 uint32_t unaligned_chunk_size) {
  uint32_t uncompressed_size = read_big_endian(stream);

  if (uncompressed_size == 0) {
    throw NotImplementedException(
        "Uncompressed size is 0 but LitT chunk exists");
  }

  // since we've already read the uncompressed size from the chunk
  uint32_t compressed_size = unaligned_chunk_size - 4;
  std::vector<uint8_t> compressed_data(compressed_size);
  stream.read(reinterpret_cast<char *>(compressed_data.data()),
              compressed_size);

  std::vector<uint8_t> uncompressed_data(uncompressed_size);

  z_stream literal_stream = {
      .next_in = compressed_data.data(),
      .avail_in = compressed_size,
      .next_out = uncompressed_data.data(),
      .avail_out = uncompressed_size,
      .zalloc = Z_NULL,
      .zfree = Z_NULL,
  };

  const int init_result = inflateInit(&literal_stream);

  if (init_result != Z_OK) {
    throw std::runtime_error("Could not inflate literal table");
  }

  const int inflate_result = inflate(&literal_stream, false);

  if (inflate_result != Z_STREAM_END) {
    if (inflate_result == Z_OK) {
      throw std::runtime_error(
          "zlib inflate was successful, but did not complete fully");
    }

    throw std::runtime_error("zlib inflate failed");
  }

  uint8_t *curr_pos = uncompressed_data.data();
  uint32_t num_literals = big_endian_from_bytes<uint32_t>(curr_pos);
  curr_pos += 4;

  std::vector<ErlTerm> terms;
  LOG(INFO) << "num_literals: " << num_literals << std::endl;

  for (uint32_t i = 0; i < num_literals; i++) {
    [[maybe_unused]]
    uint32_t literal_size = big_endian_from_bytes<uint32_t>(curr_pos);
    curr_pos += 4;

    auto result = ErlTerm::from_binary(curr_pos);
    LOG(INFO) << i << ": " << result.first.display() << std::endl;
    terms.push_back(result.first);

    curr_pos = result.second;
  }

  return LiteralChunk(std::move(terms));
}

BeamFile read_chunks(const std::string &filename) {
  std::ifstream input(filename, std::ios::binary);
  input.exceptions(std::ifstream::failbit | std::ifstream::badbit);

  if (!input) {
    std::cerr << "Error opening file" << std::endl;
    exit(1);
  }

  const std::string version = read_string(input, 4);
  const uint32_t _total_length [[maybe_unused]] = read_big_endian(input);
  const std::string beam_text = read_string(input, 4);

  auto atom_chunk = std::optional<AtomChunk>();
  auto code_chunk = std::optional<CodeChunk>();
  auto literal_chunk = std::optional<LiteralChunk>();

  while (input.peek() != EOF) {
    const std::string module_name = read_string(input, 4);
    const uint32_t raw_size = read_big_endian(input);

    LOG(INFO) << std::format("module: {}, size: {}", module_name, raw_size)
              << std::endl;

    const uint32_t aligned_chunk_len = (4 * ((raw_size + 3) / 4));
    const auto chunk_start = input.tellg();

    if (module_name == "AtU8") {
      atom_chunk = parse_atom_chunk(input);

    } else if (module_name == "Code") {
      try {
        // use size, not the aligned chunk size here
        code_chunk = parse_code_chunk(
            input, chunk_start + static_cast<std::streamoff>(raw_size));
      } catch (NotImplementedException *e) {
        LOG(WARNING) << e->what() << std::endl;
      }

    } else if (module_name == "Atom") {
      throw NotImplementedException(
          "Doesn't handle files with latin1 encoding");

    } else if (module_name == "LitT") {
      literal_chunk = parse_literal_chunk(input, raw_size);
    }

    const auto chunk_end =
        chunk_start + static_cast<std::streamoff>(aligned_chunk_len);

    input.seekg(chunk_end);
  }

  return BeamFile(*atom_chunk, *code_chunk, *literal_chunk);
}

int main(int argc, char *argv[]) {
  // setup logging
  FLAGS_log_dir = "/tmp/logs";
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(argv[0]);

  // parse beam file
  if (argc != 2) {
    LOG(FATAL) << "Needs a filename argument" << std::endl;
  }

  const std::string filename = argv[1];
  const auto chunks = read_chunks(filename);
}
