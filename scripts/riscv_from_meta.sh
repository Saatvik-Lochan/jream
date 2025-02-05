#!/usr/bin/env bash
set -euo pipefail

file="$1"
callable_fun_file="$2"

name="${file%_m.S}"
asm_file="$name.S"

script_dir=$(dirname "$0")

# do the meta compilation step
python $script_dir/meta_assembly_compile.py $file $callable_fun_file

# assemble
$script_dir/get_binary.sh $asm_file
