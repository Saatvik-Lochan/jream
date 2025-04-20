#include "../include/beamparser.hpp"
#include "../include/execution.hpp"
#include "../include/setup_logging.hpp"
#include "../include/profiler.hpp"
#include <cstring>
#include <stdexcept>

#ifdef EXEC_LOG
#pragma message("EXEC_LOG is defined")
#endif

int main(int argc, char *argv[]) {
  PROFILE_INIT();
  PROFILE();

  setup_logging(argv[0]);
  DLOG(INFO) << "Logging Enabled";

  if (argc < 3) {
    throw std::logic_error("Not enough arguments. Usage: ./trial <beam_file> "
                           "<func_name>");
  }

  auto file_name = argv[1];

  auto beamfile = read_chunks(file_name);
  beamfile.log();

#ifdef EXEC_LOG
  beamfile.log();
#endif

  DLOG(INFO) << "File read and parsed";

  emulator_main.register_beam_sources({&beamfile});

  // get the file name
  auto last_dot = strrchr(file_name, '.');
  *last_dot = '\0';
  auto last_sep = strrchr(file_name, '/');

  char *module = last_sep == NULL ? file_name : last_sep + 1;

  GlobalFunctionId start_function = {
      .module = module, .function_name = argv[2], .arity = 0};

  emulator_main.run(start_function);

  DLOG(INFO) << "Complete";
}
