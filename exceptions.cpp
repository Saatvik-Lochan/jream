#include "exceptions.h"

NotImplementedException::NotImplementedException(const char* message) : std::logic_error(message) {};