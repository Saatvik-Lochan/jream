#include "setup_logging.h"

void setup_logging(char *filename) {
  FLAGS_log_dir = "/tmp";
  fLI::FLAGS_logbufsecs = 0;
  
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(filename);
}
