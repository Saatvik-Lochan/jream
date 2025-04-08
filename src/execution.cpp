#include <algorithm>
#include <cstdlib>
#include <glog/logging.h>

#include <cassert>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <strings.h>
#include <sys/mman.h>
#include <unordered_map>

#include "asm_callable.hpp"
#include "asm_utility.hpp"
#include "beam_defs.hpp"
#include "bif.hpp"
#include "execution.hpp"
#include "generated/instr_code.hpp"
#include "op_arity.hpp"
#include "pcb.hpp"
#include "precompiled.hpp"
#include "riscv_gen.hpp"

std::optional<uintptr_t> bif_from_id(GlobalFunctionIdentifier id,
                                     const AtomChunk &atom_chunk) {

  auto &atoms = atom_chunk.atoms;

  auto id_string = std::format("{}:{}/{}", atoms[id.module],
                               atoms[id.function_name], id.arity);
  auto result = name_bif_map.find(id_string);

  if (result == name_bif_map.end()) {
    return std::nullopt;
  }

  return std::make_optional(result->second);
}

// garbage collection is tricky
inline std::vector<uint8_t> translate_code_section(CodeChunk &code_chunk,
                                                   CodeSection code_sec) {
  std::vector<uint8_t> compiled;
  std::unordered_map<uint64_t, size_t> label_offsets;

  // convenience lambdas
  auto add_code = [&compiled](const std::vector<uint8_t> &code) {
    compiled.insert(compiled.end(), code.begin(), code.end());
  };

  auto add_riscv_instr = [&compiled](RISCV_Instruction instr) {
    compiled.insert(compiled.end(), instr.raw, instr.raw + 4);
  };

  auto add_riscv_instrs =
      [&add_riscv_instr](const std::vector<RISCV_Instruction> &code) {
        std::for_each(code.begin(), code.end(), add_riscv_instr);
      };

  // reserve a spot which will need to be filled
  struct LinkRequest {
    ssize_t branch_instr_location;
    uint64_t label;
  };

  std::vector<LinkRequest> link_requests;

  // must be called before adding the branch instr
  auto reserve_branch_label = [&link_requests, &compiled](uint64_t label) {
    ssize_t curr_offset = compiled.size();
    link_requests.push_back(
        LinkRequest{.branch_instr_location = curr_offset, .label = label});
  };

  // test lambdas
  auto test_stack_type = [&](Instruction instr, AsmSnippet stack_check) {
    auto label = instr.arguments[0];
    assert(label.tag == LABEL_TAG);

    auto label_val = label.arg_raw.arg_num;

    auto source = instr.arguments[1];

    // load into t0
    add_riscv_instrs(create_load_appropriate(source, 5));
    add_code(get_riscv(stack_check));

    reserve_branch_label(label_val);
    add_riscv_instr(create_branch_not_equal(6, 7, 0));
  };

  auto test_heap_type = [&](Instruction instr, AsmSnippet stack_check,
                            AsmSnippet heap_check) {
    auto label = instr.arguments[0];
    assert(label.tag == LABEL_TAG);

    auto label_val = label.arg_raw.arg_num;

    auto source = instr.arguments[1];

    // load into t0
    add_riscv_instrs(create_load_appropriate(source, 5));
    add_code(get_riscv(stack_check));

    reserve_branch_label(label_val);
    add_riscv_instr(create_branch_not_equal(6, 7, 0));

    add_code(get_riscv(heap_check));

    reserve_branch_label(label_val);
    add_riscv_instr(create_branch_not_equal(6, 7, 0));
  };

  auto add_comparison = [&](Instruction instr, uint8_t branch_funct3) {
    auto label = instr.arguments[0];
    assert(label.tag == LABEL_TAG);

    auto label_val = label.arg_raw.arg_num;

    auto arg1 = instr.arguments[1];
    auto arg2 = instr.arguments[2];

    // load into a0, a1
    add_riscv_instrs(create_load_appropriate(arg1, 10));
    add_riscv_instrs(create_load_appropriate(arg2, 11));

    add_code(get_riscv(DO_COMP_SNIP));

    reserve_branch_label(label_val);

    // branch based of if return is lt/ge/ne/eq 0
    add_riscv_instr(
        create_B_type_instruction(0b1100011, branch_funct3, 10, 0, 0));
  };

  const auto add_setup_args_code =
      [&add_riscv_instr, &code_chunk](std::initializer_list<uint64_t> args) {
        auto arg_arr = new uint64_t[args.size()];
        std::copy(args.begin(), args.end(), arg_arr);

        auto &compacted = code_chunk.compacted_arg_p_array;

        auto index = compacted.size();
        compacted.push_back(arg_arr);

        // load the pointer at index'th value in the argument array (pointer
        // to this in s2=x18) to the s3=x19 register
        add_riscv_instr(create_load_doubleword(19, 18, index * 8));
      };

  auto add_call_biff = [&](std::span<Argument> bif_args, uint64_t fail_label,
                           Argument dest_reg, uint64_t bif_num,
                           size_t instr_index) {
    for (size_t i = 0; i < bif_args.size(); i++) {
      // load into a0, ..., aN
      add_riscv_instrs(create_load_appropriate(bif_args[i], 10 + i));
    }

    const auto func_id = code_chunk.import_table_chunk->imports[bif_num];
    const auto result = bif_from_id(func_id, *code_chunk.atom_chunk);

    if (!result) {
      throw std::logic_error(
          std::format("BIF '{}' was called, but is not implemented", bif_num));
    }

    add_setup_args_code({*result});
    add_code(get_riscv(BIF_SNIP));

    if (fail_label) {
      reserve_branch_label(fail_label);
      // i.e. if a1 is not 0, then branch
      add_riscv_instr(create_branch_not_equal(11, 0, 0));
    }

    // store a0 in dest_reg
    add_riscv_instrs(create_store_appropriate(dest_reg, 10));
  };

  // main loop
  for (size_t instr_index = code_sec.start; instr_index < code_sec.end;
       instr_index++) {

    const auto &instr = code_chunk.instructions[instr_index];

    switch (instr.op_code) {
    case DEBUG_EXECUTE_ARBITRARY: {
      auto function_pointer = instr.arguments[0];
      auto flag_pos = instr.arguments[1];

      assert(function_pointer.tag == LITERAL_TAG);
      assert(flag_pos.tag == LITERAL_TAG);

      add_setup_args_code(
          {function_pointer.arg_raw.arg_num, flag_pos.arg_raw.arg_num});

      add_code(get_riscv(DEBUG_EXECUTE_ARIBITRARY_SNIP));
      break;
    }
    case LABEL_OP: {
      auto label_arg = instr.arguments[0];
      assert(label_arg.tag == LITERAL_TAG);
      label_offsets[label_arg.arg_raw.arg_num] = compiled.size();
      break;
    }

    case LINE_OP: { // ignore, debug info
      break;
    }

    case FUNC_INFO_OP: { // could assert on function identifier
      break;
    }

    case MOVE_OP: {
      auto source = instr.arguments[0];
      auto destination = instr.arguments[1];

      add_riscv_instrs(create_load_appropriate(source, 5));
      add_riscv_instrs(create_store_appropriate(destination, 5));

      break;
    }

    case SWAP_OP: {
      auto reg1 = instr.arguments[0];
      auto reg2 = instr.arguments[1];

      add_riscv_instrs(create_load_appropriate(reg1, 5));
      add_riscv_instrs(create_load_appropriate(reg2, 6));

      add_riscv_instrs(create_store_appropriate(reg1, 6));
      add_riscv_instrs(create_store_appropriate(reg2, 5));

      break;
    }

    case ALLOCATE_OP: {
      auto alloc_amount = instr.arguments[0];
      assert(alloc_amount.tag == LITERAL_TAG);

      add_setup_args_code({alloc_amount.arg_raw.arg_num});
      add_code(get_riscv(ALLOCATE_SNIP));
      break;
    }

    case DEALLOCATE_OP: {
      auto alloc_amount = instr.arguments[0];
      assert(alloc_amount.tag == LITERAL_TAG);

      add_setup_args_code({alloc_amount.arg_raw.arg_num});
      add_code(get_riscv(DEALLOCATE_SNIP));
      break;
    }

    case INIT_YREGS_OP: {
      // extended list
      auto yregs = instr.arguments[0];
      assert(yregs.tag == EXT_LIST_TAG);

      auto yregs_val = yregs.arg_raw.arg_vec_p;

      add_code(get_riscv(INIT_YREGS_SNIP));

      constexpr uint8_t t0 = 5;
      constexpr uint8_t t1 = 6;
      constexpr uint8_t s1 = 9;

      add_riscv_instr(create_load_doubleword(t1, s1, STOP * 8));
      for (const auto &reg_to_save : *yregs_val) {
        assert(reg_to_save.tag == Y_REGISTER_TAG);

        const auto y_reg_num = reg_to_save.arg_raw.arg_num;

        add_riscv_instr(create_store_doubleword(t1, t0, (y_reg_num + 1) * 8));
      };

      break;
    }

    case CALL_OP: {
      [[maybe_unused]]
      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      [[maybe_unused]]
      auto label = instr.arguments[1];
      assert(label.tag == LABEL_TAG);

      add_setup_args_code({label.arg_raw.arg_num});

      // check reductions and maybe yield
      add_code(get_riscv(CALL_SNIP));
      break;
    }

    case CALL_LAST_OP: {

      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      auto label = instr.arguments[1];
      assert(label.tag == LABEL_TAG);

      auto deallocate = instr.arguments[2];
      assert(deallocate.tag == LITERAL_TAG);

      add_setup_args_code({label.arg_raw.arg_num, deallocate.arg_raw.arg_num});

      // check reductions and maybe yield
      add_code(get_riscv(CALL_LAST_SNIP));
      break;
    }

    case BIF0_OP: {
      auto bif_num = instr.arguments[0];
      assert(bif_num.tag == LITERAL_TAG);

      auto bif_num_val = bif_num.arg_raw.arg_num;

      auto destination = instr.arguments[1];

      add_call_biff({}, 0, destination, bif_num_val, instr_index);
      break;
    }

      // BIF1 and 2 are quite similar, maybe refactor
    case GC_BIF1_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);
      auto label_val = label.arg_raw.arg_num;

      auto bif_num = instr.arguments[2];
      assert(bif_num.tag == LITERAL_TAG);

      auto bif_num_val = bif_num.arg_raw.arg_num;

      Argument args[] = {instr.arguments[3]};

      auto destination = instr.arguments[4];

      add_call_biff(args, label_val, destination, bif_num_val, instr_index);

      break;
    }

    case GC_BIF2_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);
      auto label_val = label.arg_raw.arg_num;

      auto bif_num = instr.arguments[2];
      assert(bif_num.tag == LITERAL_TAG);

      auto bif_num_val = bif_num.arg_raw.arg_num;

      Argument args[] = {instr.arguments[3], instr.arguments[4]};

      auto destination = instr.arguments[5];

      add_call_biff(args, label_val, destination, bif_num_val, instr_index);

      break;
    }

    case CALL_EXT_OP: {
      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      // only 8 argument register!
      assert(arity.arg_raw.arg_num < 8);

      for (size_t i = 0; i < arity.arg_raw.arg_num; i++) {
        // s5 (x21) has the x_registers
        // we load them into a0, ..., an
        add_riscv_instr(create_load_x_reg(10 + i, 0, 21));
      }

      auto destination = instr.arguments[1];
      assert(destination.tag == LITERAL_TAG);

      auto index = destination.arg_raw.arg_num;
      auto func_id = code_chunk.import_table_chunk->imports[index];

      auto result = bif_from_id(func_id, *code_chunk.atom_chunk);

      if (result) {
        add_setup_args_code({*result});
        add_code(get_riscv(CALL_EXT_BIF_SNIP));
      } else {
        // external call which is either not implemented or user defined
        // if external then what?
        throw std::logic_error("Bif not yet defined");
      }
      break;
    }

    // code is pretty much a duplicate of the above
    // TODO deduplicate once you've implemented external module calling
    case CALL_EXT_ONLY_OP: {

      auto arity = instr.arguments[0];
      assert(arity.tag == LITERAL_TAG);

      assert(arity.arg_raw.arg_num < 8);

      for (size_t i = 0; i < arity.arg_raw.arg_num; i++) {
        // s5 (x21) has the x_registers
        // we load them into a0, ..., an
        add_riscv_instr(create_load_x_reg(10 + i, 0, 21));
      }

      auto destination = instr.arguments[1];
      assert(destination.tag == LITERAL_TAG);

      auto index = destination.arg_raw.arg_num;
      auto func_id = code_chunk.import_table_chunk->imports[index];

      auto result = bif_from_id(func_id, *code_chunk.atom_chunk);

      if (result) {
        add_setup_args_code({*result});
        add_code(get_riscv(CALL_EXT_BIF_SNIP));
        add_code(get_riscv(RETURN_SNIP));
      } else {
        // external call which is either not implemented or user defined
        // if external then what?
        throw std::logic_error("Bif not yet defined");
      }
      break;
    }

    case RETURN_OP: {
      // load code pointer and jump
      add_code(get_riscv(RETURN_SNIP));
      break;
    }

    case BADMATCH_OP: {
      // TODO return the value?
      add_code(get_riscv(BADMATCH_SNIP));
      break;
    }

    case REMOVE_MESSAGE_OP: {
      add_code(get_riscv(REMOVE_SNIP));
      break;
    }

    case WAIT_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);

      add_setup_args_code({label.arg_raw.arg_num});
      add_code(get_riscv(WAIT_SNIP));
      break;
    }

    case SEND_OP: {
      add_code(get_riscv(SEND_SNIP));
      break;
    }

    case LOOP_REC_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);

      add_code(get_riscv(LOOP_REC_1_SNIP));
      reserve_branch_label(label.arg_raw.arg_num);

      // beq t0, t1, LABEL
      add_riscv_instr(create_branch_equal(5, 6, 0));

      add_code(get_riscv(LOOP_REC_2_SNIP));

      auto destination = instr.arguments[1];

      // save t2
      add_riscv_instrs(create_store_appropriate(destination, 7));
      break;
    }

    case PUT_LIST_OP: {
      auto head = instr.arguments[0];
      auto tail = instr.arguments[1];
      auto destination = instr.arguments[2];

      // head goes in t1, tail goes in t2
      add_riscv_instrs(create_load_appropriate(head, 5));
      add_riscv_instrs(create_load_appropriate(tail, 6));

      add_code(get_riscv(PUT_LIST_SNIP));

      add_riscv_instrs(create_store_appropriate(destination, 5));

      break;
    }

    case GET_LIST_OP: {
      // load the register pointed at in t0
      add_riscv_instrs(create_load_appropriate(instr.arguments[0], 5));

      add_code(get_riscv(GET_LIST_SNIP));

      // store from t1 and t2
      add_riscv_instrs(create_store_appropriate(instr.arguments[1], 6));
      add_riscv_instrs(create_store_appropriate(instr.arguments[2], 7));
      break;
    }

    case GET_TUPLE_ELEMENT_OP: {
      auto source = instr.arguments[0];
      auto element = instr.arguments[1];
      assert(element.tag == LITERAL_TAG);
      auto destination = instr.arguments[2];

      add_setup_args_code({element.arg_raw.arg_num});

      // load source into t0
      add_riscv_instrs(create_load_appropriate(source, 5));
      add_code(get_riscv(GET_TUPLE_ELEMENT_SNIP));

      add_riscv_instrs(create_store_appropriate(destination, 5));
      break;
    }

    case MAKE_FUN3_OP: {
      auto index = instr.arguments[0];
      assert(index.tag == LITERAL_TAG);

      auto freeze_list = instr.arguments[2];
      assert(freeze_list.tag == EXT_LIST_TAG);

      auto freeze_list_val = freeze_list.arg_raw.arg_vec_p;

      auto num_free = freeze_list_val->size();

      add_setup_args_code({index.arg_raw.arg_num});

      // now we alloc heap space and store index
      // store num spots to allocate in t0
      add_riscv_instr(create_add_immediate(5, 0, num_free + 2));
      add_code(get_riscv(MAKE_FUN3_SNIP));

      // now pointing to pos right after the index loc
      int position = 16;

      for (const auto &reg_to_save : *freeze_list_val) {
        // load reg to t0
        add_riscv_instrs(create_load_appropriate(reg_to_save, 5));

        // t1 has the heap pointer
        // sd t0, pos(t1)
        add_riscv_instr(create_store_doubleword(6, 5, position));

        position += 8;
      }

      // the value in t1 (i.e. the heap pointer before this)
      add_riscv_instrs(create_store_appropriate(instr.arguments[1], 6));

      break;
    }

    case IS_GE_OP: {
      // funct3 for lt, since we branch when ge
      add_comparison(instr, 0x4);
      break;
    }

    case IS_LT_OP: {
      // funct3 for ge, since we branch when ge
      add_comparison(instr, 0x5);
      break;
    }

    case IS_TUPLE_OP: {
      test_heap_type(instr, IS_TUPLE_1_SNIP, IS_TUPLE_2_SNIP);

      break;
    }

    case IS_NONEMPTY_LIST_OP: {
      test_stack_type(instr, IS_NONEMPTY_LIST_SNIP);
      break;
    }

    case IS_NIL_OP: {
      test_stack_type(instr, IS_NIL_SNIP);
      break;
    }

    case TEST_ARITY_OP: {
      auto label = instr.arguments[0];
      assert(label.tag == LABEL_TAG);

      auto source = instr.arguments[1];

      auto arity = instr.arguments[2];
      assert(arity.tag == LITERAL_TAG);

      // setup args
      add_setup_args_code({arity.arg_raw.arg_num});

      // load soucre into t1
      add_riscv_instrs(create_load_appropriate(source, 6));
      add_code(get_riscv(TEST_ARITY_SNIP));

      auto label_val = label.arg_raw.arg_num;
      reserve_branch_label(label_val);

      // bne t0, t1
      add_riscv_instr(create_branch_not_equal(5, 6, 0));

      break;
    }

    default: {
      throw std::logic_error(std::format("Opcode {} not implemented yet",
                                         op_names[instr.op_code]));
    }
    }
  }

  // fix link requests
  for (auto request : link_requests) {
    auto index = request.branch_instr_location;
    auto data_loc = &compiled[index];
    auto as_riscv_instr = reinterpret_cast<RISCV_Instruction *>(data_loc);

    // check if label is inside function scope
    assert(label_offsets.contains(request.label));

    ssize_t label_val = label_offsets[request.label];
    int16_t offset = label_val - index;

    set_imm_B_type_instruction(*as_riscv_instr, offset);
  }

  code_chunk.label_offsets = label_offsets;

  return compiled;
}

std::vector<uint8_t> translate_function(CodeChunk &code_chunk,
                                        uint64_t func_index) {
  assert(func_index < code_chunk.function_count);

  auto end_instr_index = code_chunk.instructions.size();
  if (func_index + 1 < code_chunk.function_count) {
    end_instr_index =
        code_chunk.label_table[code_chunk.func_label_table[func_index + 1]];
  }

  auto section = CodeSection{
      code_chunk.label_table[code_chunk.func_label_table[func_index]],
      end_instr_index};

  return translate_code_section(code_chunk, section);
}

uint8_t *move_code_to_memory(const std::vector<uint8_t> &code) {
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

  return reinterpret_cast<uint8_t *>(allocated_mem);
}

uint8_t *compile_erlang_func(CodeChunk &code_chunk, uint64_t func_index) {
  auto code = translate_function(code_chunk, func_index);
  auto code_ptr = move_code_to_memory(code);
  return code_ptr;
}

ErlReturnCode setup_and_go_label(ProcessControlBlock *pcb, uint64_t label_num) {

  auto code_chunk = pcb->get_shared<CODE_CHUNK_P>();

  return PreCompiled::setup_and_goto_label(
      code_chunk, pcb, all_funs, PreCompiled::teardown_code, label_num);
}

ErlReturnCode resume_process(ProcessControlBlock *pcb) {
  return setup_and_go_label(pcb, pcb->get_shared<RESUME_LABEL>());
}

ProcessControlBlock *create_process_entry_label(CodeChunk &code_chunk,
                                                uint64_t label) {
  ProcessControlBlock *pcb = new ProcessControlBlock;

  // TODO initialise all shared code
  pcb->set_shared<CODE_CHUNK_P>(&code_chunk);
  pcb->set_shared<XREG_ARRAY>(new ErlTerm[1001]);

  pcb->set_shared<RESUME_LABEL>(label);

  auto head = pcb->get_address<MBOX_HEAD>();
  pcb->set_shared<MBOX_TAIL>(head);
  pcb->set_shared<MBOX_SAVE>(head);

  return pcb;
}

ProcessControlBlock *create_process(CodeChunk &code_chunk,
                                    uint64_t func_index) {
  auto label_num = code_chunk.func_label_table[func_index];
  return create_process_entry_label(code_chunk, label_num);
}

ProcessControlBlock *Scheduler::pick_next() {
  auto chosen_it = runnable.begin();

  if (chosen_it == runnable.end()) {
    return nullptr;
  }

  auto value = runnable.extract(chosen_it);
  return value.value();
}

bool Scheduler::signal(ProcessControlBlock *process) {
  auto it = waiting.find(process);

  if (it == waiting.end()) {
    return false;
  }

  auto node = waiting.extract(it);
  runnable.insert(std::move(node));

  return true;
}

// TODO this can actually just go in the compilation step, I
// anyway need to find the bif_from_id there, so I might as well fill in the
// relevant thing...
void init_ext_jump(BeamSrc *file) {
  const auto &imports = file->import_table_chunk.imports;

  // allocate imports
  auto import_num = imports.size();
  auto ext_jumps = new ExtJump[import_num];

  for (size_t i = 0; i < import_num; i++) {
    const auto func_id = imports[i];

    auto result = bif_from_id(func_id, file->atom_chunk);

    if (result) {
      ext_jumps[i].func = *result;
    }
  }

  file->code_chunk.external_jump_locations = ext_jumps;
}

void create_emulator(std::vector<BeamSrc *> files) {
  Emulator out;

  // set imports/exports
  for (auto file_p : files) {
    file_p->code_chunk.import_table_chunk = &file_p->import_table_chunk;
    file_p->code_chunk.function_table_chunk = &file_p->function_table_chunk;
    file_p->code_chunk.atom_chunk = &file_p->atom_chunk;
  }
}

void run_emulator(Emulator emulator, CodeChunk &code_chunk,
                  uint64_t func_index) {

  auto &scheduler = emulator.scheduler;

  auto process = create_process(code_chunk, func_index);
  scheduler.waiting.insert(process);

  while (auto to_run = scheduler.pick_next()) {
    resume_process(to_run);
  }
}
