#include "bif.hpp"
#include "execution.hpp"
#include "external_term.hpp"
#include "pcb.hpp"

/* The functions here define the BIFs
 *
 * They must take trivial types as arguments. Pointers, uint64_t, for example.
 * If they are going to return they return an uint64_t
 *
 * If they have an issue they return but they set the value in the register
 * s9 to 1.
 */

uint64_t add() { return 100; }

uint64_t spawn_1(uint64_t fun_raw) {

  auto fun = ErlTerm(fun_raw);
  auto code_chunk_p =
      emulator_main.scheduler.executing_process->get_shared<CODE_CHUNK_P>();

  assert(fun.getTagType() == BOXED_T);
  assert(fun.getBoxedType() == FUN_T);

  auto header_ptr = fun.as_ptr();
  auto header = *header_ptr;

  auto index = header_ptr[1];
  auto &functions = code_chunk_p->function_table_chunk->functions;

  assert(index < functions.size());
  auto func_id = functions[index];

  // +1 for the index ptr
  auto size = header >> 6;
  assert(size == func_id.num_free + 1);

  auto pcb = create_process(*code_chunk_p);

  // set entry point
  pcb->set_shared<RESUME_LABEL>(func_id.label);

  auto new_x_reg = pcb->get_shared<XREG_ARRAY>();
  auto func_arity = func_id.arity - func_id.num_free;
  auto num_free = func_id.num_free;

  assert(func_arity == 0);

  // copy frozen variales
  std::copy(header_ptr + 2, header_ptr + 2 + num_free, new_x_reg + func_arity);

  // move to ready queue
  emulator_main.scheduler.runnable.insert(pcb);

  // prepare return value
  return make_pid(pcb);
}
