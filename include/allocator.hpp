
/*
 * A custom pool allocator that expects many allocs and few frees.
 *
 * Designed for allocating cons lists.
 *
 */

#include <cstddef>
#include <span>
#include <vector>

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

    auto slab_start = new T[new_slab_size];
    slabs.push_back(slab_start);

    std::span<T> slab(slab_start, new_slab_size);

    FreeObject *tail = nullptr;

    for (T &item : slab) {
      FreeObject &item_as_obj = reinterpret_cast<FreeObject&>(item);
      item_as_obj.next = tail;
      tail = &item_as_obj;
    }

    free_list_head = tail;
  }

public:
  StablePoolAllocator(size_t size = 512): base_slab_size(size) {}

  T *alloc() {
    if (free_list_head == nullptr) {
      add_and_link_slab();
    }

    auto out = free_list_head;
    free_list_head = free_list_head->next;

    return reinterpret_cast<T *>(out);
  }

  void free(T *ptr) {
    auto freed_as_obj_ptr = reinterpret_cast<FreeObject *>(ptr);
    freed_as_obj_ptr->next = free_list_head;
    free_list_head = freed_as_obj_ptr;
  }

  size_t get_free_num() {
    FreeObject *current = free_list_head;

    size_t count = 0;

    while (current != nullptr) {
      current = current->next;
      count += 1;
    }

    return count;
  }
};
