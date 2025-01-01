#include "exceptions.h"
#include "op_arity.h"
#include <cassert>
#include <cstdint>
#include <format>
#include <fstream>
#include <iosfwd>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <sys/types.h>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

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
  return (buffer[3] << 0) | (buffer[2] << 8) | (buffer[1] << 16) |
         (buffer[0] << 24);
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

  for (int i = 0; i < num_atoms; i++) {
    uint8_t atom_length = read_byte(stream);
    std::string atom_name = read_string(stream, atom_length);

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
  // EXT_LIST, -- TODO
  // EXT_FPREG,
  // EXT_ALLOC_LIST,
  // EXT_LITERAL
};

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
    std::string message = std::format(
        "Extended values are not implemented yet (byte {})", tag_byte);
    throw NotImplementedException(message.c_str());
  }
  default:
    throw std::logic_error("Cannot reach here");
  }
}

using ArgNumber = std::variant<uint64_t, std::unique_ptr<uint8_t[]>>;
ArgNumber parse_argument_number(std::ifstream &stream, uint8_t tag_byte) {

  if (tag_byte >> 3 == 0b11111) {
    uint8_t num_following_bytes = read_byte(stream);

    auto value = std::make_unique<uint8_t[]>(num_following_bytes);
    stream.read(reinterpret_cast<char *>(value.get()), num_following_bytes);

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

      return num_following_bytes;

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

CodeChunk parse_code_chunk(std::ifstream &stream) {

  auto chunk_start = stream.tellg();

  // TODO use these for optimising allocations
  const uint32_t sub_size = read_big_endian(stream);
  const uint32_t instruction_set_version = read_big_endian(stream);
  const uint32_t op_code_max = read_big_endian(stream);
  const uint32_t label_count = read_big_endian(stream);
  const uint32_t function_count = read_big_endian(stream);

  // skip till subsize amount forward
  stream.seekg(chunk_start + static_cast<std::streamoff>(sub_size));

  // read code
  std::vector<Instruction> instructions;
  instructions.reserve(label_count);

  for (uint32_t label_no = 0; label_no < label_count; label_no++) {
    uint8_t op_code = read_byte(stream);
    uint8_t arity = op_arities[op_code];

    std::vector<Argument> args(3);

    for (int i = 0; i < arity; i++) {
      uint8_t tag_byte = read_byte(stream);
      enum Tag tag = parse_tag(tag_byte);

      // NOTE assuming no extended tags here, so we must parse the same bit
      // TODO support extended tags
      ArgNumber num = parse_argument_number(stream, tag_byte);

      args[i] = std::make_pair(tag, std::move(num));
    }

    instructions.push_back(Instruction(op_code, std::move(args)));
  }

  return CodeChunk(std::move(instructions));
}

struct BeamFile {
  AtomChunk atom_chunk;
  CodeChunk code_chunk;

  BeamFile(AtomChunk atom_chunk, CodeChunk code_chunk)
      : atom_chunk(std::move(atom_chunk)), code_chunk(std::move(code_chunk)) {}
};

BeamFile read_chunks(const std::string &filename) {
  std::ifstream input(filename, std::ios::binary);

  if (!input) {
    std::cerr << "Error opening file" << std::endl;
    exit(1);
  }

  const std::string version = read_string(input, 4);
  const uint32_t total_length = read_big_endian(input);
  const std::string beam_text = read_string(input, 4);

  auto atom_chunk = std::optional<AtomChunk>();
  auto code_chunk = std::optional<CodeChunk>();

  while (!input.eof()) {
    const std::string module_name = read_string(input, 4);
    std::cout << module_name << '\n';

    const uint32_t size = read_big_endian(input);
    std::cout << size << '\n';

    const uint32_t chunk_len = (4 * ((size + 3) / 4));
    const auto chunk_start = input.tellg();

    if (module_name == "Atom")
      atom_chunk = parse_atom_chunk(input);
    if (module_name == "Code")
      code_chunk = parse_code_chunk(input);

    const auto chunk_end = chunk_start + static_cast<std::streamoff>(chunk_len);
    input.seekg(chunk_end);
  }

  return BeamFile(*atom_chunk, *code_chunk);
}

std::vector<Instruction>
parse_code_section(const std::vector<uint8_t> &code_section) {
  std::vector<Instruction> instructions;

  for (size_t chunk_index = 0; chunk_index < code_section.size();) {
    uint8_t op_code = code_section[chunk_index++];
    uint8_t arity = op_arities[op_code];

    std::vector<Argument> args(3);

    for (int i = 0; i < arity; i++) {
      uint8_t tag_byte = read_byte(std::ifstream & stream) enum Tag tag =
          parse_tag(code_section[chunk_index]);

      // NOTE assuming no extended tags here, so we must parse the same bit
      // TODO support extended tags
      ArgNumber num;
      tie(num, chunk_index) = parse_argument_number(code_section, chunk_index);

      args[i] = std::make_pair(tag, std::move(num));
    }

    instructions.push_back(Instruction(op_code, std::move(args)));
  }

  return instructions;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Needs a filename argument" << std::endl;
    return 1;
  }

  const std::string filename = argv[1];
  const auto chunks = read_chunks(filename);
  const auto code_chunk = chunks.at("Code");

  for (const auto &instruction : code_chunk) {
  }
}
