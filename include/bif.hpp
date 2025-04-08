#ifndef BIF
#define BIF

#include <cstdint>
#include <string>
#include <unordered_map>
#include <utility>

uint64_t spawn_1(uint64_t fun_raw);
uint64_t add();

#define MAP(Name, Function)                                                    \
  std::make_pair(Name, reinterpret_cast<uintptr_t>(Function))

const inline std::unordered_map<std::string, uintptr_t> name_bif_map = {
    MAP("erlang:test/0", add),
    MAP("erlang:spawn/1", spawn_1)
};

#endif
