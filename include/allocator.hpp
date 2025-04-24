
/*
 * A custom pool allocator that expects many allocs and few frees.
 *
 * Designed for allocating cons lists.
 *
 */

#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <cstddef>
#include <glog/logging.h>
#include <span>
#include <vector>

#ifdef ENABLE_MEMORY_LOG
#define MLOG(...) LOG(INFO) << __VA_ARGS__
#else
#define MLOG(...) void(0)
#endif

template <typename T> class StablePoolAllocator {

  struct FreeObject {
    FreeObject *next;
  };

  static_assert(sizeof(T) >= sizeof(FreeObject),
                "size of T must be greater than that of a free list object");

  const size_t base_slab_size;
  std::vector<T *> slabs;
  FreeObject *free_list_head = nullptr;

  void add_and_link_slab() {
    auto new_slab_size = base_slab_size << (slabs.size());
    MLOG("StablePoolAllocator: Allocating new slab " << new_slab_size);

    auto slab_start = new T[new_slab_size];
    slabs.push_back(slab_start);

    std::span<T> slab(slab_start, new_slab_size);

    FreeObject *tail = nullptr;

    for (T &item : slab) {
      FreeObject &item_as_obj = reinterpret_cast<FreeObject &>(item);
      item_as_obj.next = tail;
      tail = &item_as_obj;
    }

    free_list_head = tail;
  }

public:
  StablePoolAllocator(size_t size = 512) : base_slab_size(size) {}
  ~StablePoolAllocator() {
    for (auto slab : slabs) {
      delete[] slab;
    }

    slabs.clear();
  }

  inline T *alloc() {
    if (free_list_head == nullptr) {
      add_and_link_slab();
    }

    auto out = free_list_head;
    free_list_head = free_list_head->next;

    return reinterpret_cast<T *>(out);
  }

  inline void free(T *ptr) {
    auto freed_as_obj_ptr = reinterpret_cast<FreeObject *>(ptr);
    freed_as_obj_ptr->next = free_list_head;
    free_list_head = freed_as_obj_ptr;
  }

  inline size_t get_free_num() {
    FreeObject *current = free_list_head;

    size_t count = 0;

    while (current != nullptr) {
      current = current->next;
      count += 1;
    }

    return count;
  }
};

template <typename T> class StableBumpAllocator {
  T *next_free = nullptr;
  T *top = nullptr;

  const size_t base_slab_size;
  std::vector<std::span<T>> slabs;

public:
  StableBumpAllocator(size_t size = 512) : base_slab_size(size) {}
  ~StableBumpAllocator() {
    for (auto slab : slabs) {
      delete[] slab.data();
    }
  }

  inline T *alloc(size_t size) {
    if (!next_free || next_free + size > top) {
      auto new_slab_size = base_slab_size << (slabs.size());
      MLOG("StableBumpAlocator: Allocating a new slab of size "
           << new_slab_size);
      auto new_slab = new T[new_slab_size];
      next_free = new_slab;
      top = new_slab + new_slab_size;
      slabs.emplace_back(std::span(new_slab, new_slab_size));
    }

    auto out = next_free;
    next_free += size;

    return out;
  }

  inline bool contains(T *ptr) {
    for (auto slab : slabs) {
      auto start = slab.data();
      auto end = slab.data() + slab.size();

      if (start <= ptr && ptr < end) {
        return true;
      }
    }

    return false;
  }
};

#endif
