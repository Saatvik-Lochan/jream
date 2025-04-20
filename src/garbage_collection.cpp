#include "garbage_collection.hpp"
#include "external_term.hpp"
#include "profiler.hpp"
#include <algorithm>
#include <glog/logging.h>
#include <span>
#include <stack>


constexpr uint64_t MOVED_CONS_MARKER = 37 << 2;

/*
 * Currently only minor gc is implemented and old heap cannot free just yet.
 */

// we assume to_space has enough size
YoungHeap minor_gc(const std::vector<std::span<ErlTerm>> &root_set,
                   ErlTerm *to_space, const YoungHeap current_young,
                   GeneralPurposeHeap &old_heap) {
  PROFILE();

  ErlTerm *new_top = to_space;

  auto alloc_and_copy = [&new_top](std::span<ErlTerm> to_copy) {
    std::ranges::copy(to_copy, new_top);

    auto out = new_top;
    new_top += to_copy.size();

    return out;
  };

  std::stack<std::span<ErlTerm>> old_heap_to_fix;

  auto copy_term = [&alloc_and_copy, &current_young, &old_heap,
                    &old_heap_to_fix](ErlTerm *ptr_term_ptr) {
    ErlTerm ptr_term = *ptr_term_ptr;

    ErlTerm *copy_ptr = ptr_term.as_ptr();

    switch (ptr_term & 0b11) {
    case 0b11:
    case 0b00: {
      return;
    }
    case 0b01: {
      if (!current_young.contains(copy_ptr)) {
        // i.e. it is in the old heap and we don't have to gc it
        return;
      }

      // list case
      if (copy_ptr[0] == MOVED_CONS_MARKER) {
        // i.e list already moved
        *ptr_term_ptr = copy_ptr[1];
        return;
      }

      std::span<ErlTerm> span(copy_ptr, 2);

      ErlTerm *new_ref;

      // TODO must do an iterative search here to move all necessary values to
      // the old heap
      if (current_young.is_old_here(copy_ptr)) {
        new_ref = old_heap.allocate_cons();
        std::ranges::copy(span, new_ref);

        old_heap_to_fix.push({new_ref, 2});

      } else {
        new_ref = alloc_and_copy(std::span<ErlTerm>{copy_ptr, 2});
      }

      auto new_erl_ref = make_cons(new_ref);

      copy_ptr[0] = MOVED_CONS_MARKER; // 0ing out to mark as moved
      copy_ptr[1] = new_erl_ref;

      *ptr_term_ptr = new_erl_ref;
      return;
    }
    case 0b10: {

      if (!current_young.contains(copy_ptr)) {
        return;
      }

      // boxed case
      ErlTerm header = *copy_ptr; // a.k.a header when relevant

      if ((header & 0b11) == 0b11) {
        constexpr uint64_t MASK = ~0UL << 1;
        // i.e. head has already been moved
        // we just zero out the last bit to get a valid boxed tag
        *ptr_term_ptr = header & MASK;
        return;
      }

      size_t size = (header >> 6) + 1; // size including header

      ErlTerm *new_ref;
      std::span<ErlTerm> span(copy_ptr, size);

      if (current_young.is_old_here(copy_ptr)) {
        new_ref = old_heap.allocate_other(size);
        std::ranges::copy(span, new_ref);

        old_heap_to_fix.push(span);
      } else {
        new_ref = alloc_and_copy(span);
      }

      // mark that the value was moved to new_ref
      *copy_ptr = tag(new_ref, std::bitset<2>(0b11));

      *ptr_term_ptr = make_boxed(new_ref);
      return;
    }
    }
  };

  for (auto root : root_set) {
    for (auto &e : root) {
      copy_term(&e);
    }
  }

  while (!old_heap_to_fix.empty()) {
    auto next = old_heap_to_fix.top();
    old_heap_to_fix.pop();

    for (auto &e : next) {
      copy_term(&e);
    }
  }

  ErlTerm *new_bot = to_space;
  while (new_bot <= new_top) {
    copy_term(new_bot);
    new_bot += 1;
  }

  // garbage collection should be done
  return YoungHeap{
      .heap_start = to_space, .heap_top = new_top, .highwater = new_top};
}
