#!/usr/bin/env bash
set -euo pipefail

file="$1"

name="${file%_m.S}"
asm_file="$name.S"

# do the meta compilation step
python meta_assembly_compile.py $file

# assemble
./get_binary.sh $asm_file
