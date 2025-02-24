import sys
import os
import subprocess
from pathlib import Path

directory = Path(sys.argv[1])
assert (directory.is_dir())

out_file = Path(sys.argv[2])
assert (out_file.is_file() or not out_file.exists())

callable_fun_file = Path(sys.argv[3])

generated_tag = f"// THIS FILE WAS GENERATED BY {Path(__file__).name}"

starting_lines = """
#include <glog/logging.h>

#include "instr_code.h"

std::vector<uint8_t> get_riscv(OpCode op) {
  switch (op) {
"""

ending_lines = """
  default:
    LOG(FATAL) << "opCode " << op << " has not been implemented yet";
  }
}
"""


def case_from_file(file: Path):
    assert (file.is_file())

    script_dir = os.path.dirname(os.path.abspath(__file__))
    script_path = os.path.join(script_dir, 'riscv_from_meta.sh')
    result = subprocess.run([script_path, file, callable_fun_file],
                            text=True, capture_output=True)

    if result.returncode != 0:
        print(result.stderr)
        exit()

    byte_array_str = result.stdout.strip()

    # get enum name from file
    enum_name = f"{file.name[:-4]}_op".upper()

    case_text = (
        f"  case {enum_name}:\n"
        f"    return {byte_array_str};\n"
    )

    return case_text


all_lines = [starting_lines]

for file in directory.glob("*_m.S"):
    all_lines.append(case_from_file(file))

all_lines.append(ending_lines)

with open(out_file, "w") as f:
    f.writelines(all_lines)
