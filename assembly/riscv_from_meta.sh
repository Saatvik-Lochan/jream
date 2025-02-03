#!/usr/bin/env bash
set -euo pipefail

file="$1"

name="${file%_m.S}"
asm_file="$name.S"
object="$name.o"
binary="$name.bin"

# do the meta compilation step
python meta_assembly_compile.py $file

# assemble
riscv64-linux-gnu-as -o $object $asm_file
riscv64-linux-gnu-objcopy -O binary $object $binary

# format it into C-style hex
bytes="{ $(xxd -p $binary | sed 's/\(..\)/0x\1, /g' | sed 's/,$//') }"
echo $bytes

# clean up
rm $object 
rm $binary
