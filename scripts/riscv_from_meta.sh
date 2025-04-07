#!/usr/bin/env bash
# This file is no longer used in code gen
set -euo pipefail

file="$1"
callable_fun_file="$2"

file_dir=$(dirname "$file")
name=$(basename "$file")
new_name="${name%_m.S}.S"
asm_file="$file_dir/build/$new_name"

script_dir=$(dirname "$0")

# do the meta compilation step
python "$script_dir"/meta_assembly_compile.py "$file" "$callable_fun_file"

# assemble
"$script_dir"/get_binary.sh "$asm_file"
