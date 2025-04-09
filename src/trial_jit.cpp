#include "../include/setup_logging.hpp"
#include "../include/beamparser.hpp"
#include "../include/execution.hpp"

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);

  auto beamfile = read_chunks(argv[1]);
}
