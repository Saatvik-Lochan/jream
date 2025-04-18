#include "../include/beamparser.hpp"
#include "../include/execution.hpp"
#include "../include/setup_logging.hpp"
#include <stdexcept>

#ifdef EXEC_LOG
#pragma message("EXEC_LOG is defined")
#endif

int main(int argc, char *argv[]) {
  setup_logging(argv[0]);
  DLOG(INFO) << "Logging Enabled";

  if (argc < 3) {
    throw std::logic_error("Not enough arguments. Usage: ./trial <beam_file> "
                           "<func_name>");
  }

  auto beamfile = read_chunks(argv[1]);
  beamfile.log();

#ifdef EXEC_LOG
  beamfile.log();
#endif

  DLOG(INFO) << "File read and parsed";

  emulator_main.register_beam_sources({&beamfile});

  GlobalFunctionId start_function = {
      .module = "merge", .function_name = argv[2], .arity = 0};

  emulator_main.run(start_function);

  DLOG(INFO) << "Complete";
}
