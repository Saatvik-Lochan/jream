#ifndef SETUP_LOGGING_H
#define SETUP_LOGGING_H

#include "profiler.hpp"
#include <glog/logging.h>

inline void setup_logging(char *filename) {
  PROFILE();
  FLAGS_log_prefix = false;
  FLAGS_log_dir = "./logs";
  fLI::FLAGS_logbufsecs = 0;
  
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(filename);
}

#endif
