#include "exceptions.h"
#include "external_term.h"
#include "int_from_bytes.h"
#include "op_arity.h"
#include <cassert>
#include <cstdint>
#include <format>
#include <fstream>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <sys/types.h>
#include <utility>
#include <variant>
#include <vector>
#include <zlib.h>

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

struct Chunk {
  virtual ~Chunk() = default;
};

struct AtomChunk : Chunk {
  std::vector<std::string> atoms;

  AtomChunk(std::vector<std::string> atoms) : atoms(std::move(atoms)) {}
};

AtomChunk parse_atom_chunk(std::ifstream &stream) {
  uint32_t num_atoms = read_big_endian(stream);
  std::vector<std::string> atoms;

  std::cout << "Atoms:" << std::endl;

  for (uint32_t i = 0; i < num_atoms; i++) {
    uint8_t atom_length = read_byte(stream);
    std::string atom_name = read_string(stream, atom_length);

    std::cout << "\t" << atom_name << std::endl;

    atoms.push_back(atom_name);
  }

  return AtomChunk(std::move(atoms));
}

enum Tag {
  LITERAL,
  INTEGER,
  ATOM,
  X_REGISTER,
  Y_REGISTER,
  LABEL,
  CHARACTER,
  EXT_LIST,
  EXT_FPREG,
  EXT_ALLOC_LIST,
  EXT_LITERAL
};

constexpr std::string TagToString(Tag tag) {
  switch (tag) {
  case LITERAL:
    return "Literal";
  case INTEGER:
    return "Integer";
  case ATOM:
    return "Atom";
  case X_REGISTER:
    return "X Register";
  case Y_REGISTER:
    return "Y Register";
  case LABEL:
    return "Label";
  case CHARACTER:
    return "Character";
  case EXT_LIST:
    return "Extended List";
  case EXT_FPREG:
    return "Extended Floating Point Register";
  case EXT_ALLOC_LIST:
    return "Extended Alloc List";
  case EXT_LITERAL:
    return "Extended Literal";
  default:
    throw std::logic_error("unknown tag");
  }
}

enum Tag parse_tag(uint8_t tag_byte) {
  const uint8_t mask = 0b111;

  switch (tag_byte & mask) {
  case 0b000:
    return LITERAL;
  case 0b001:
    return INTEGER;
  case 0b010:
    return ATOM;
  case 0b011:
    return X_REGISTER;
  case 0b100:
    return Y_REGISTER;
  case 0b101:
    return LABEL;
  case 0b110:
    return CHARACTER;
  case 0b111: {
    const uint8_t ext_bits = tag_byte >> 4;

    switch (ext_bits) {
    case 0b0001:
      return EXT_LIST;
    case 0b0010:
      return EXT_FPREG;
    case 0b0011:
      return EXT_ALLOC_LIST;
    case 0b0100:
      return EXT_LITERAL;
    default:
      throw std::logic_error(std::format("No such tag: {:08b}", tag_byte));
    }
  }
  default:
    throw std::logic_error("Cannot reach here");
  }
}

// TODO refer to beam_disasm.erl for fix
using ArgNumber = std::variant<uint64_t, std::vector<uint8_t>>;
ArgNumber parse_argument_number(std::ifstream &stream, uint8_t tag_byte) {

  if (tag_byte >> 3 == 0b11111) {
    auto next_byte = read_byte(stream);
    auto tag = parse_tag(next_byte);

    assert(tag == LITERAL);

    // TODO maybe support bigger numbers, I can't imagine when you would get
    // something this big though
    auto embedded_size =
        std::get<uint64_t>(parse_argument_number(stream, next_byte));

    std::vector<uint8_t> value(embedded_size);
    stream.read(reinterpret_cast<char *>(value.data()), embedded_size);

    return value;
  }

  if (tag_byte & 1 << 3) {
    if (tag_byte & 1 << 4) {

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

    const uint64_t value = tag_byte >> 3;
    return value;
  }
}

ArgNumber parse_extended_literal(std::ifstream &stream) {
  auto next_byte = read_byte(stream);
  auto tag = parse_tag(next_byte);
  assert(tag == LITERAL);

  auto embedded_size = parse_argument_number(stream, next_byte);
  return embedded_size;
}

using Argument = std::pair<enum Tag, ArgNumber>;

struct Instruction {
  uint8_t opCode;
  std::vector<Argument> arguments;

  Instruction(uint8_t opCode, std::vector<Argument> args)
      : opCode(opCode), arguments(std::move(args)) {}
};

struct CodeChunk : Chunk {
  std::vector<Instruction> instructions;

  CodeChunk(std::vector<Instruction> instructions)
      : instructions(std::move(instructions)) {}
};

CodeChunk parse_code_chunk(std::ifstream &stream, std::streampos chunk_end) {

  // TODO use these for optimising allocations
  const uint32_t sub_size = read_big_endian(stream);
  std::cout << "sub_size: " << sub_size << std::endl;

  auto chunk_start = stream.tellg();

  const uint32_t _instruction_set_version [[maybe_unused]] =
      read_big_endian(stream);
  const uint32_t _op_code_max [[maybe_unused]] = read_big_endian(stream);

  const uint32_t label_count = read_big_endian(stream);
  std::cout << std::format("label count: {}", label_count) << std::endl;

  const uint32_t _function_count [[maybe_unused]] = read_big_endian(stream);

  // skip till subsize amount forward
  stream.seekg(chunk_start + static_cast<std::streamoff>(sub_size));

  // read code
  std::vector<Instruction> instructions;
  instructions.reserve(label_count);

  while (stream.tellg() < chunk_end) {
    uint8_t op_code = read_byte(stream);
    uint8_t arity = op_arities[op_code];

    std::cout << std::format("name: {}, op_code: {}, arity: {} ",
                             op_names[op_code], op_code, arity)
              << std::endl;

    std::vector<Argument> args(3);

    for (int i = 0; i < arity; i++) {
      uint8_t tag_byte = read_byte(stream);
      enum Tag tag = parse_tag(tag_byte);

      std::cout << "\t" << TagToString(tag) << ": ";

      switch (tag) {
      case LITERAL:
      case INTEGER:
      case ATOM:
      case X_REGISTER:
      case Y_REGISTER:
      case LABEL:
      case CHARACTER: {
        auto index = parse_argument_number(stream, tag_byte);
        std::cout << std::get<uint64_t>(index);
        break;
      }
      case EXT_LITERAL: {
        auto index = parse_extended_literal(stream);
        std::cout << "index " << std::get<uint64_t>(index);
        break;
      }
      case EXT_LIST:
      case EXT_FPREG:
      case EXT_ALLOC_LIST: {
        std::string error_msg =
            std::format("Tag '{}' not implemented yet", TagToString(tag));
        throw NotImplementedException(error_msg.c_str());
      }
      default:
        throw std::logic_error("invalid tag");
      }

      std::cout << std::endl;

      // NOTE assuming no extended tags here, so we must parse the same bit
      // TODO support extended tags
      ArgNumber num = parse_argument_number(stream, tag_byte);

      args[i] = std::make_pair(tag, std::move(num));
    }

    instructions.push_back(Instruction(op_code, std::move(args)));
  }

  return CodeChunk(std::move(instructions));
}

struct LiteralChunk {
  std::vector<ErlTerm> literals;

  LiteralChunk(std::vector<ErlTerm> literals) : literals(std::move(literals)) {}
};

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
  std::cout << "num_literals: " << num_literals << std::endl;

  for (uint32_t i = 0; i < num_literals; i++) {
    [[maybe_unused]]
    uint32_t literal_size = big_endian_from_bytes<uint32_t>(curr_pos);
    curr_pos += 4;

    auto result = ErlTerm::from_binary(curr_pos);
    std::cout << i << ": " << result.first.display() << std::endl;
    terms.push_back(result.first);

    curr_pos = result.second;
  }

  return LiteralChunk(std::move(terms));
}

struct BeamFile {
  AtomChunk atom_chunk;
  CodeChunk code_chunk;

  BeamFile(AtomChunk atom_chunk, CodeChunk code_chunk)
      : atom_chunk(std::move(atom_chunk)), code_chunk(std::move(code_chunk)) {}
};

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

  while (input.peek() != EOF) {
    const std::string module_name = read_string(input, 4);
    const uint32_t raw_size = read_big_endian(input);

    std::cout << std::format("module: {}, size: {}", module_name, raw_size)
              << std::endl;

    const uint32_t aligned_chunk_len = (4 * ((raw_size + 3) / 4));
    const auto chunk_start = input.tellg();

    if (module_name == "AtU8")
      atom_chunk = parse_atom_chunk(input);

    else if (module_name == "Code") {
      try {
        // use size, not the aligned chunk size here
        code_chunk = parse_code_chunk(
            input, chunk_start + static_cast<std::streamoff>(raw_size));
      } catch (NotImplementedException *e) {
        std::cout << e->what() << std::endl;
      }

    } else if (module_name == "Atom")
      throw NotImplementedException(
          "Doesn't handle files with latin1 encoding");

    else if (module_name == "LitT")
      parse_literal_chunk(input, raw_size);

    const auto chunk_end =
        chunk_start + static_cast<std::streamoff>(aligned_chunk_len);

    input.seekg(chunk_end);
  }

  return BeamFile(*atom_chunk, *code_chunk);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Needs a filename argument" << std::endl;
    return 1;
  }

  const std::string filename = argv[1];
  const auto chunks = read_chunks(filename);
  const auto code_chunk = chunks.code_chunk;
}
