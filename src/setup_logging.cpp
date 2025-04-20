#include "setup_logging.hpp"
#include "profiler.hpp"

void setup_logging(char *filename) {
  PROFILE();
  FLAGS_log_prefix = false;
  FLAGS_log_dir = "/tmp";
  fLI::FLAGS_logbufsecs = 0;
  
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(filename);
}
