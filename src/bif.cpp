#include "bif.hpp"
#include "execution.hpp"
#include "external_term.hpp"

void spawn_1(ErlTerm *x_regs, CodeChunk *code_chunk_p) {
  auto fun = x_regs[0];
  assert(fun.getTagType() == BOXED_T);
  assert(fun.getBoxedType() == FUN_T);

  auto header_ptr = fun.as_ptr();
  auto header = *header_ptr;

  auto index = header_ptr[1];
  auto func_id = code_chunk_p->function_table_chunk->functions[index];

  // +1 for the index ptr
  auto size = header >> 6;
  assert(size == func_id.num_free + 1);

  auto pcb = create_process(*code_chunk_p);

  // set entry point
  pcb->set_shared<RESUME_LABEL>(func_id.label);

  auto new_x_reg = pcb->get_shared<XREG_ARRAY>();
  auto func_arity = func_id.arity - func_id.num_free;
  auto num_free = func_id.num_free;

  // copy func arguments
  std::copy(x_regs, x_regs + func_arity, new_x_reg);

  // copy frozen variales
  std::copy(header_ptr + 1, header_ptr + 1 + num_free, new_x_reg + func_arity);

  // move to ready queue
  emulator_main.scheduler.runnable.insert(pcb);
}
