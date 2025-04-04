#ifndef BIF
#define BIF

#include "execution.hpp"
#include "external_term.hpp"
#include <string>
#include <unordered_map>

void spawn_1(ErlTerm *, CodeChunk *);

typedef void (*ext_func)(ErlTerm *x_reg_array, CodeChunk *code_chunk_p);

const inline std::unordered_map<std::string, ext_func> bif_to_name = {
    std::make_pair("erlang:spawn/1", spawn_1),
};

#endif
