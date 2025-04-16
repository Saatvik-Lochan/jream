#ifndef GARBAGE_COLLECTION_H
#define GARBAGE_COLLECTION_H

#include "allocator.hpp"
#include "external_term.hpp"

class OldHeap {
  StablePoolAllocator<std::array<ErlTerm, 2>> list_allocator;

public:
  inline ErlTerm *allocate_cons() { return list_allocator.alloc()->begin(); };
  inline ErlTerm *allocate_other(size_t size) { return new ErlTerm[size]; }
};

struct YoungHeap {
  ErlTerm *heap_start;
  ErlTerm *heap_top;
  ErlTerm *highwater;

  bool contains(ErlTerm *ptr) const {
    return heap_start <= ptr && ptr <= heap_top;
  }

  bool is_old_here(ErlTerm *ptr) const { return ptr <= highwater; }
};

YoungHeap minor_gc(const std::vector<std::span<ErlTerm>> &root_set,
                   ErlTerm *to_space, const YoungHeap current_young,
                   OldHeap &old_heap);

#endif
