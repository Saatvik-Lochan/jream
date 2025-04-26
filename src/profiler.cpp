#include "profiler.hpp"

#ifdef ENABLE_PROFILING
thread_local std::vector<Frame> Tracer::stack;
#endif
