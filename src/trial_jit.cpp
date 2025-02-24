#include "../include/beam_defs.h"
#include "../include/execution.h"
#include "../include/setup_logging.h"

Argument get_lit(uint64_t arg) {
  return Argument{LITERAL_TAG, {.arg_num = arg}};
}

CodeChunk create_code_chunk(const std::vector<Instruction> instructions) {
  return CodeChunk(std::move(instructions), 0, 0, {}, {});
}

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);
  // given
  auto instructions = {Instruction{ALLOCATE_OP, {get_lit(3)}}};
  auto code_chunk = create_code_chunk(std::move(instructions));

  ErlTerm e[5];

  ProcessControlBlock pcb;
  pcb.set_shared<STOP>(e);

  // when
  run_code_section(code_chunk, CodeSection{0, 1}, &pcb);

  // then
  [[maybe_unused]] auto val = pcb.get_shared<STOP>();
}
