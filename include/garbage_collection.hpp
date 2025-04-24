#ifndef GARBAGE_COLLECTION_H
#define GARBAGE_COLLECTION_H

#include "allocator.hpp"
#include "external_term.hpp"
#include <algorithm>

class GeneralPurposeHeap {
  StableBumpAllocator<ErlTerm> bump_allocator;

public:
  inline ErlTerm *allocate(size_t size) {
    return bump_allocator.alloc(size);
  }

  inline bool contains(ErlTerm *ptr) {
    return bump_allocator.contains(ptr);
  };
};

struct YoungHeap {
  ErlTerm *heap_start;
  ErlTerm *heap_top;
  ErlTerm *highwater;
  std::span<std::span<ErlTerm>> frags;

  bool contains(ErlTerm *ptr) const {
    return (heap_start <= ptr && ptr <= heap_top) ||
           std::ranges::any_of(frags, [&ptr](auto span) {
             return span.data() <= ptr && ptr < span.data() + span.size();
           });
    ;
  }

  bool is_old_here(ErlTerm *ptr) const {
#ifdef ENABLE_GENERATIONAL_GC
    return heap_start <= ptr && ptr < highwater;
#else
    return false;
#endif
  }
};

YoungHeap minor_gc(const std::vector<std::span<ErlTerm>> &root_set,
                   std::span<ErlTerm> to_space, const YoungHeap current_young,
                   GeneralPurposeHeap &old_heap);

#endif
