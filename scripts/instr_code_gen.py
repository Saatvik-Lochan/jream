import sys
import os
import subprocess
from pathlib import Path
from meta_assembly_compile import do_compile

project_root = Path(sys.argv[1])
callable_fun_file = Path(sys.argv[2])


def enum_and_case_from(file: Path):
    assert (file.is_file())

    script_dir = os.path.dirname(os.path.abspath(__file__))
    script_path = os.path.join(script_dir, 'get_binary.sh')
    result = subprocess.run([script_path, file, callable_fun_file],
                            text=True, capture_output=True)

    if result.returncode != 0:
        print(result.stderr)
        exit(1)

    byte_array_str = result.stdout.strip()

    # get enum name from file
    enum_name = f"{file.name[:-2]}_SNIP".upper()
    enum_text = f"  {enum_name}, // {file.relative_to(project_root)}\n"

    variable_name = enum_name.lower()
    variable_text = f"  static constexpr uint8_t {
        variable_name}[] = {byte_array_str};\n"

    case_text = (
        f"  case {enum_name}:\n"
        f"    return {variable_name};\n\n"
    )

    return enum_text, variable_text, case_text


generated_tag = f"// THIS FILE WAS GENERATED BY {
    Path(__file__).relative_to(project_root)}\n"

header_out_file = Path(sys.argv[4])
assert (header_out_file.is_file() or not header_out_file.exists())

header_starting_lines = """#ifndef INSTR_CODE_H
#define INSTR_CODE_H

#include <cstdint>
#include <stdexcept>
#include <span>

enum AsmSnippet {
"""

header_func_start = """};

inline std::span<const uint8_t> get_riscv(AsmSnippet snippet) {
"""

header_switch_start = """
  switch (snippet) {
"""

header_ending_lines = """  default:
    throw std::logic_error("Invalid AsmSnippet");
  }
}

#endif
"""

asm_directory = Path(sys.argv[3])
assert (asm_directory.is_dir())

build_directory = asm_directory / "build"
build_directory.mkdir(exist_ok=True)

# clear build dir
for file in build_directory.iterdir():
    if file.is_file():
        file.unlink()

for file in asm_directory.glob("*_m.S"):
    do_compile(file, callable_fun_file)

header_lines = [generated_tag, header_starting_lines]
variable_lines = []
switch_case_lines = []

for file in build_directory.glob("*.S"):
    enum_option, variable_line, switch_case = enum_and_case_from(file)
    header_lines.append(enum_option)
    variable_lines.append(variable_line)
    switch_case_lines.append(switch_case)

header_lines.append(header_func_start)

for var_line in variable_lines:
    header_lines.append(var_line)

header_lines.append(header_switch_start)

for case in switch_case_lines:
    header_lines.append(case)

header_lines.append(header_ending_lines)

with open(header_out_file, "w") as f:
    f.writelines(header_lines)
