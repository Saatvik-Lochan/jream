#include "../include/execution.hpp"
#include "../include/setup_logging.hpp"
#include <cstring>
#include <stdexcept>

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);

  if (argc < 2) {
    throw std::logic_error(
        "Not enough arguments. Usage: ./trial <func_string> ");
  }

  emulator_main.read_and_execute(argv[1]);
}
