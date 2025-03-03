#include "exceptions.hpp"

NotImplementedException::NotImplementedException(const char *message)
    : std::logic_error(message) {};
