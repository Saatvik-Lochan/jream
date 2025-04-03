#include "../include/beam_defs.hpp"
#include "../include/setup_logging.hpp"
#include "../include/beamparser.hpp"

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

CodeChunk create_code_chunk(const std::vector<Instruction> instructions) {
  return CodeChunk(std::move(instructions), 0, 0);
}

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);

  auto beamfile = read_chunks(argv[1]);
  auto code_chunk = beamfile.code_chunk;
}
