#ifndef BIF
#define BIF

#include "external_term.hpp"
#include "generated/instr_code.hpp"
#include <cstdint>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>

// debugging utility
uintptr_t curr_cp();
size_t num_highwater();

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

BIFReturn test_add(uint64_t a, uint64_t b);
BIFReturn test_mul10(uint64_t a);
BIFReturn ret100();
BIFReturn test_fail(uint64_t a, uint64_t b);

BIFReturn spawn_1(uint64_t fun_raw);
BIFReturn length(uint64_t list_raw);
BIFReturn self();
BIFReturn erl_div(int64_t a, int64_t b);
BIFReturn erl_sub(int64_t a, int64_t b);
BIFReturn erl_add(int64_t a, int64_t b);
BIFReturn erl_bxor(int64_t a, int64_t b);
BIFReturn erl_bsr(int64_t a, int64_t amount);

BIFReturn list_split(uint64_t first_size_raw, uint64_t list_raw, uint64_t xregs);

BIFReturn file_consult(uint64_t file_name, uint64_t xregs);
BIFReturn io_write(uint64_t term);

#define MAP(Name, Function)                                                    \
  std::make_pair(Name, reinterpret_cast<uintptr_t>(Function))

#define PAIR(Name, Snip)                                                    \
  std::make_pair(Name, Snip)

const inline std::unordered_map<std::string, uintptr_t> name_bif_map = {
    // testing BIFs
    MAP("erlang:test/0", ret100), 
    MAP("erlang:test/1", test_mul10), 
    MAP("erlang:test/2", test_add),
    MAP("erlang:test_fail/2", test_fail),

    MAP("erlang:spawn/1", spawn_1),
    MAP("erlang:length/1", length),
    MAP("erlang:self/0", self),
    MAP("erlang:div/2", erl_div),

    MAP("file:consult/1", file_consult),
    MAP("io:write/1", io_write),
};

const inline std::unordered_map<std::string, AsmSnippet> inline_bif_map = {
    PAIR("erlang:+/2", ERL_ADD_SNIP),
    PAIR("erlang:-/2", ERL_SUB_SNIP),
    PAIR("erlang:bsr/2", ERL_BSR_SNIP),
    PAIR("erlang:bxor/2", ERL_BXOR_SNIP),
};

#endif
