#include "setup_logging.h"

void setup_logging(char *filename) {
  FLAGS_log_dir = "/tmp";
  google::FlushLogFiles(google::INFO);
  google::InitGoogleLogging(filename);
}
