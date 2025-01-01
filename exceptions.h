#ifndef EXCEPTIONS
#define EXCEPTIONS

#include <stdexcept>
class NotImplementedException : public std::logic_error {
  public:
    NotImplementedException(const char* message);
};

#endif
