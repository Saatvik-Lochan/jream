#ifndef PROFILER_H
#define PROFILER_H

#ifdef ENABLE_PROFILING

#include <cassert>
#include <chrono>
#include <fstream>
#include <mutex>
#include <string>
#include <vector>

using Clock = std::chrono::steady_clock;

struct Frame {
  std::string_view name;
  Clock::time_point start;
  uint64_t child_us = 0; // time spent in callees
};

class Tracer {
public:
  static void push(std::string_view name) {
    get_stack().push_back({name, Clock::now(), 0});
  }

  static void pop() {
    auto &st = get_stack();
    assert(!st.empty() && "Tracer::pop without matching push");

    Frame fr = st.back();
    st.pop_back();

    const uint64_t total_us =
        std::chrono::duration_cast<std::chrono::microseconds>(Clock::now() -
                                                              fr.start)
            .count();
    const uint64_t self_us = total_us - fr.child_us; // subtract children

    // Build folded stack
    std::string &buf = get_thread_buffer();
    for (const Frame &f : st) {
      buf.append(f.name);
      buf.push_back(';');
    }
    buf.append(fr.name);
    buf.push_back(' ');
    buf.append(std::to_string(self_us));
    buf.push_back('\n');

    // Propagate total time to parent so it can subtract us later
    if (!st.empty())
      st.back().child_us += total_us;

    // Flush once in a while to keep memory bounded
    if (buf.size() > 32 * 1024)
      flush();
  }

  // call once at program end (or use atexit)
  static void flush() {
    std::lock_guard<std::mutex> lock(get_mutex());
    std::ofstream &out = get_ofstream();
    out.write(get_thread_buffer().data(), get_thread_buffer().size());
    get_thread_buffer().clear();
  }

private:
  // one stack per thread
  inline static thread_local std::vector<Frame> stack_;
  static std::vector<Frame> &get_stack() { return stack_; }

  // one aggregation buffer per thread
  inline static thread_local std::string buffer_;
  static std::string &get_thread_buffer() { return buffer_; }

  // shared sink
  static std::ofstream &get_ofstream() {
    static std::ofstream out("trace.folded", std::ios::trunc);
    return out;
  }
  static std::mutex &get_mutex() {
    static std::mutex m;
    return m;
  }

  // make sure flush runs at thread exit too
  struct Flusher {
    ~Flusher() { Tracer::flush(); }
  };
  inline static thread_local Flusher flusher_;
};

class Profiler {
public:
  explicit Profiler(std::string_view n) { Tracer::push(n); }
  ~Profiler() { Tracer::pop(); }
};

#define PROFILE() Profiler _p(__func__)
#define NPROFILE(Name) Profiler _p(Name)

#else

#define PROFILE()
#define NPROFILE(name) ((void)0)
#endif
#endif
