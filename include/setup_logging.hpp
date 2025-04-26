#ifndef SETUP_LOGGING_H
#define SETUP_LOGGING_H

#include "fstream"
#include "profiler.hpp"
#include <filesystem>
#include <glog/logging.h>
#include <iostream>
#include <unordered_map>

class ThreadLogSink : public google::LogSink {
public:
  ThreadLogSink(std::filesystem::path logs_dir) : logs_dir(logs_dir) {
    if (std::filesystem::exists(logs_dir) &&
        std::filesystem::is_directory(logs_dir)) {
      // Iterate over all entries in the directory
      for (const auto &entry : std::filesystem::directory_iterator(logs_dir)) {
        // If it's a regular file, remove it
        auto filename = entry.path().filename().string();

        if (std::filesystem::is_regular_file(entry.status()) &&
            filename.starts_with("log_thread_")) {
          std::filesystem::remove(entry.path());
        }
      }
    } else {
      std::cerr << "The directory does not exist or is not a valid directory!"
                << std::endl;
    }
  }

  // This method will be called whenever a log is generated
  virtual void send(google::LogSeverity severity, const char *full_filename,
                    const char *base_filename, int line,
                    const google::LogMessageTime &time, const char *message,
                    size_t message_len) override {

    std::thread::id thread_id = std::this_thread::get_id();

    // Check if the file stream for this thread already exists
    if (log_files_.find(thread_id) == log_files_.end()) {
      // Create a file name based on the thread id
      std::ostringstream file_name;
      file_name << "log_thread_" << thread_id << ".log";

      auto full_filename = logs_dir / file_name.str();

      // Open the log file for appending and store the file stream
      log_files_[thread_id] =
          std::make_unique<std::ofstream>(full_filename, std::ios_base::app);
      if (!log_files_[thread_id]->is_open()) {
        std::cerr << "Failed to open log file: " << file_name.str()
                  << std::endl;
        return;
      }
    }

    // Write the log message to the file
    (*log_files_[thread_id]) << message << std::endl;
  }

private:
  std::unordered_map<std::thread::id, std::unique_ptr<std::ofstream>>
      log_files_;

  const std::filesystem::path logs_dir;
};

inline void setup_logging(char *filename) {
  PROFILE();
  google::InitGoogleLogging(filename);

  auto sink = new ThreadLogSink("./logs");
  google::AddLogSink(sink);
}

#endif
