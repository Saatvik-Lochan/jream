#include "../include/beamparser.hpp"
#include "../include/execution.hpp"
#include "../include/setup_logging.hpp"

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);

  auto beamfile = read_chunks(argv[1]);
  beamfile.log();

  emulator_main.register_beam_sources({&beamfile});

  GlobalFunctionId start_function = {
      .module = "merge", .function_name = "main", .arity = 0};

  emulator_main.run(start_function);
}
