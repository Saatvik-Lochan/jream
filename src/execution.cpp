#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <stdexcept>
#include <sys/mman.h>

#include "asm_callable.h"
#include "asm_utility.h"
#include "beam_defs.h"
#include "execution.h"
#include "instr_code.h"
#include "op_arity.h"
#include "pcb.h"

uint64_t *get_compact_and_cache_instr_args(const CodeChunk &code_chunk,
                                           size_t index) {

  auto &c_args_ptr = code_chunk.compacted_arg_p_array[index];
  const auto &args = code_chunk.instructions[index].arguments;

  if (c_args_ptr != nullptr) {
    return c_args_ptr;
  }

  auto num_args = args.size();

  c_args_ptr = new uint64_t[num_args];

  // do compaction
  for (size_t i = 0; i < num_args; i++) {
    const auto &argument = args[i];

    // not implemented yet
    assert(argument.tag != EXT_LIST_TAG && argument.tag != EXT_ALLOC_LIST_TAG);

    c_args_ptr[i] = argument.arg_raw.arg_num;
  }

  return c_args_ptr;
}

// garbage collection is tricky
std::vector<uint8_t> translate_function(const CodeChunk &code_chunk,
                                        CodeSection code_sec) {
  std::vector<uint8_t> compiled;
  std::unordered_map<uint64_t, size_t> label_pointers;

  // TODO handle last function. Fix the bounds of this loop
  // Maybe a start and end index
  for (size_t instr_index = code_sec.start; instr_index < code_sec.end;
       instr_index++) {

    get_compact_and_cache_instr_args(code_chunk, instr_index);

    const auto &instr = code_chunk.instructions[instr_index];

    switch (instr.opCode) {
    case LABEL_OP: {
      auto label_arg = instr.arguments[0];
      assert(label_arg.tag == LITERAL_TAG);
      label_pointers[label_arg.arg_raw.arg_num] = compiled.size();
      break;
    }

    case LINE_OP: { // ignore, debug info
      break;
    }

    case FUNC_INFO_OP: { // could assert on function identifier
      break;
    }
    default: {
      // load the pointer at index'th value in the argument array (pointer to
      // this in s2=x18) to the s3=x19 register
      auto load_instr = create_load_doubleword(19, 18, instr_index * 8);
      compiled.insert(compiled.end(), load_instr.raw, load_instr.raw + 4);

      // get translated code
      auto result = get_riscv(instr.opCode);
      compiled.insert(compiled.end(), result.begin(), result.end());
    }
    }
  }

  // TODO fix the setup and teardown (we need to save s3 as well)!
  auto setup_code = {
      0x13, 0x01, 0x81, 0xfe, // addi sp, sp, -24
      0x23, 0x30, 0x91, 0x00, // sd s1, 0(sp)
      0x23, 0x34, 0x21, 0x01, // sd s2, 8(sp)
      0x23, 0x38, 0x11, 0x00, // sd ra, 16(sp)
      0x93, 0x04, 0x05, 0x00, // mv s1, a0
      0x13, 0x89, 0x05, 0x00, // mv s2, a1
  };

  auto teardown_code = {
      0x83, 0x34, 0x01, 0x00, // ld s1, 0(sp)
      0x03, 0x39, 0x81, 0x00, // ld s2, 8(sp)
      0x83, 0x30, 0x01, 0x01, // ld ra, 16(sp)
      0x13, 0x01, 0x81, 0x01, // addi sp, sp, 24
      0x67, 0x80, 0x00, 0x00, // ret
  };

  compiled.insert(compiled.begin(), setup_code.begin(), setup_code.end());
  compiled.insert(compiled.end(), teardown_code.begin(), teardown_code.end());

  return compiled;
}

RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs, int16_t imm) {
  // since that's the size of immediate we can directly inject
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rd && rd < 32);
  assert(0 <= rs && rs < 32);

  uint32_t instr = 0;
  constexpr auto load_instr_bits = 0b0000011; // ld
  constexpr auto funct3_bits = 0b011;         // ld
  constexpr auto load_and_funct = load_instr_bits | (funct3_bits << 12);

  const auto dest_reg_bits = rd & 0b11111;
  const auto source_reg_bits = rs & 0b11111;

  instr |= load_and_funct;
  instr |= dest_reg_bits << 7;
  instr |= source_reg_bits << 15;
  instr |= imm << 20;

  RISCV_Instruction out;
  memcpy(out.raw, &instr, 4);

  return out;
}

void spawn_process(const CodeChunk &code_chunk, FunctionIdentifier f_id) {
  // initialise memory
  //  i.e. stack + heap (and old heap)
  // cache code?
  // load code
  // execute code

  // have to deal with reducing reductions on a call as well, then
  // do more scheduling shit
}

compiled_func_p move_code_to_memory(const std::vector<uint8_t> &code) {
  // allocate page aligned memory
  void *const allocated_mem = mmap(0, code.size(), PROT_READ | PROT_WRITE,
                                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (allocated_mem == MAP_FAILED) {
    std::string msg =
        std::format("Could not allocate memory with mmap. Errno: {}", errno);
    throw std::runtime_error(msg);
  }

  std::copy(code.begin(), code.end(),
            reinterpret_cast<uint8_t *>(allocated_mem));

  // make memory executable
  const auto result =
      mprotect(allocated_mem, code.size(), PROT_READ | PROT_EXEC);

  if (result == -1) {
    std::string msg =
        std::format("Could not make memory executable. Errno: {}", errno);
    throw std::runtime_error(msg);
  }

  return reinterpret_cast<compiled_func_p>(allocated_mem);
}

void run_code_section(CodeChunk &code_chunk, const CodeSection code_sec,
                      ProcessControlBlock *pcb) {
  auto &cached = code_chunk.cached_code_sections;

  if (!cached.contains(code_sec)) {
    auto code = translate_function(code_chunk, code_sec);
    LOG(INFO) << code;

    auto func = move_code_to_memory(code);
    cached[code_sec] = func;
  }

  compiled_func_p func = cached[code_sec];
  func(pcb, all_funs, code_chunk.compacted_arg_p_array);
}
