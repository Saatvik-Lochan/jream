#ifndef BIF
#define BIF

#include <cstdint>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>

struct BIFReturn {
  uint64_t a0;
  uint64_t a1;

  BIFReturn(uint64_t a) : a0(a), a1(0) {};
  BIFReturn() : a0(0), a1(1) {};
};

static_assert(std::is_standard_layout_v<BIFReturn>,
              "BIFReturn must be standard layout");
static_assert(sizeof(BIFReturn) == 16, "BIFReturn must be 16 bytes");

inline BIFReturn fail() { return BIFReturn(); }

BIFReturn spawn_1(uint64_t fun_raw);
BIFReturn add(uint64_t a, uint64_t b);
BIFReturn mul10(uint64_t a);
BIFReturn ret100();
BIFReturn test_fail(uint64_t a, uint64_t b);

#define MAP(Name, Function)                                                    \
  std::make_pair(Name, reinterpret_cast<uintptr_t>(Function))

const inline std::unordered_map<std::string, uintptr_t> name_bif_map = {
    MAP("erlang:test/0", ret100), 
    MAP("erlang:test/1", mul10), 
    MAP("erlang:test/2", add),
    MAP("erlang:test_fail/2", test_fail),
    MAP("erlang:spawn/1", spawn_1)
};

#endif
