#ifndef PROFILER_H
#define PROFILER_H

#include <mutex>
#ifdef ENABLE_PROFILING

#include <chrono>
#include <fstream>
#include <string>
#include <vector>

using Frame = std::pair<std::string, std::chrono::steady_clock::time_point>;

class Tracer {
public:
  static void push(const std::string &name) {
    get_stack().emplace_back(name, std::chrono::steady_clock::now());
  }

  static void pop() {
    auto &stack = get_stack();
    if (!stack.empty()) {
      auto [name, start] = stack.back();
      stack.pop_back();

      auto dur = std::chrono::duration_cast<std::chrono::microseconds>(
                     std::chrono::steady_clock::now() - start)
                     .count();

      std::string folded;
      for (const auto &[fn, _] : stack) {
        folded += fn + ";";
      }
      folded += name;

      std::lock_guard<std::mutex> lock(get_mutex());
      std::ofstream out("trace.folded", std::ios::app);
      out << folded << " " << dur << "\n";
    }
  }

private:
  static thread_local std::vector<Frame> stack;

  static std::vector<Frame> &get_stack() { return stack; }

  static std::mutex &get_mutex() {
    static std::mutex m;
    return m;
  }
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
