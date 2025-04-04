#ifndef BIF
#define BIF

#include "external_term.hpp"
#include <string>
#include <unordered_map>

void spawn(ErlTerm *);

typedef void (*ext_func)(ErlTerm *x_reg_array);

#define LINK(S, F) std::make_pair(S, F)

const inline std::unordered_map<std::string, ext_func> bif_to_name = {
    std::make_pair("erlang:spawn/1", spawn),
};

#endif
