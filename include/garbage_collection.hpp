#ifndef GARBAGE_COLLECTION_H
#define GARBAGE_COLLECTION_H

#include "allocator.hpp"
#include "external_term.hpp"
#include <algorithm>
#include <unordered_set>

class GeneralPurposeHeap {
  StablePoolAllocator<std::array<ErlTerm, 2>> list_allocator;
  std::unordered_set<ErlTerm *> others_alloced;

public:
  inline ErlTerm *allocate_cons() { return list_allocator.alloc()->begin(); };
  inline ErlTerm *allocate_other(size_t size) {
    auto alloced = new ErlTerm[size];
    others_alloced.insert(alloced);
    return alloced;
  }

  inline bool contains_other(ErlTerm *ptr) {
    return others_alloced.contains(ptr);
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
    return heap_start <= ptr && ptr < highwater;
  }
};

YoungHeap minor_gc(const std::vector<std::span<ErlTerm>> &root_set,
                   ErlTerm *to_space, const YoungHeap current_young,
                   GeneralPurposeHeap &old_heap);

#endif
