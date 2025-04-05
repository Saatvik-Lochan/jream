#ifndef BIF
#define BIF

#include "beam_defs.hpp"
#include "external_term.hpp"
#include <string>
#include <unordered_map>

void spawn_1(ErlTerm *, CodeChunk *);
void add(ErlTerm *, CodeChunk *);

const inline std::unordered_map<std::string, ext_func> name_bif_map = {
    std::make_pair("erlang:test/0", add),  // not a real bif
    std::make_pair("erlang:spawn/1", spawn_1),
};

#endif
