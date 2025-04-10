#ifndef BIF
#define BIF

#include "external_term.hpp"
#include <cstdint>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>

struct BIFReturn {
  uint64_t a0;
  uint64_t a1;

  BIFReturn(uint64_t a) : a0(a), a1(0) {};
  BIFReturn(ErlTerm a) : a0(a.term), a1(0) {};
  BIFReturn() : a0(0), a1(1) {};
};

static_assert(std::is_standard_layout_v<BIFReturn>,
              "BIFReturn must be standard layout");
static_assert(sizeof(BIFReturn) == 16, "BIFReturn must be 16 bytes");

inline BIFReturn fail() { return BIFReturn(); }

BIFReturn add(uint64_t a, uint64_t b);
BIFReturn mul10(uint64_t a);
BIFReturn ret100();
BIFReturn test_fail(uint64_t a, uint64_t b);

BIFReturn spawn_1(uint64_t fun_raw);
BIFReturn length(uint64_t list_raw);
BIFReturn self();
BIFReturn erl_div(uint64_t a, uint64_t b);
BIFReturn list_split(uint64_t first_size_raw, uint64_t list_raw);

BIFReturn file_consult(uint64_t file_name);
BIFReturn io_write(uint64_t term);

#define MAP(Name, Function)                                                    \
  std::make_pair(Name, reinterpret_cast<uintptr_t>(Function))

const inline std::unordered_map<std::string, uintptr_t> name_bif_map = {
    // testing BIFs
    MAP("erlang:test/0", ret100), 
    MAP("erlang:test/1", mul10), 
    MAP("erlang:test/2", add),
    MAP("erlang:test_fail/2", test_fail),

    MAP("erlang:spawn/1", spawn_1),
    MAP("erlang:length/1", length),
    MAP("erlang:self/0", self),
    MAP("erlang:div/2", erl_div),

    MAP("file:consult/1", erl_div),
    MAP("io:write/1", erl_div),

    MAP("lists:split/2", list_split),
};

#endif
