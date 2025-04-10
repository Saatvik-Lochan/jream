#include "setup_logging.hpp"

void setup_logging(char *filename) {
  FLAGS_log_prefix = false;
  FLAGS_log_dir = "/tmp";
  fLI::FLAGS_logbufsecs = 0;
  
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(filename);
}
