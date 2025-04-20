#ifndef PROFILER_H
#define PROFILER_H

#ifdef ENABLE_PROFILING

#include <chrono>
#include <fstream>
#include <string>
#include <vector>

typedef std::pair<std::string, std::chrono::steady_clock::time_point>
    TracerFrame;

// doesn't work in multithreaded situation
class Tracer {
public:
  static void push(const std::string &name) {
    stack.emplace_back(name, std::chrono::steady_clock::now());
  }

  static void pop() {
    if (!stack.empty()) {
      auto [name, start] = stack.back();
      stack.pop_back();

      // Measure time in microseconds
      auto dur = std::chrono::duration_cast<std::chrono::microseconds>(
                     std::chrono::steady_clock::now() - start)
                     .count();

      // Create the full stack string
      std::string folded;
      for (const auto &[fn, _] : stack) {
        folded += fn + ";";
      }
      folded += name;

      // Emit the line
      std::ofstream out("trace.folded", std::ios::app);
      out << folded << " " << dur << "\n";
    }
  }

private:
  static std::vector<TracerFrame> stack;
};

class Profiler {
public:
  Profiler(const std::string &name) { Tracer::push(name); }

  ~Profiler() { Tracer::pop(); }
};

#define PROFILE() Profiler _profiler(__FUNCTION__)
#define PROFILE_INIT() std::ofstream ofs("trace.folded", std::ios::trunc);

#else

#define PROFILE() ((void)0)
#define PROFILE_INIT() ((void)0)

#endif
#endif
