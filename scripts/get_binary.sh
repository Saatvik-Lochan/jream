#!/usr/bin/env bash
set -euo pipefail

asm_file="$1"
name="${asm_file%.S}"
object="$name.o"
binary="$name.bin"

# assemble
riscv64-linux-gnu-as -o $object $asm_file
riscv64-linux-gnu-objcopy -O binary $object $binary

# format it into C-style hex
bytes="{ $(xxd -p $binary | tr -d '\n' | sed 's/\(..\)/0x\1, /g' | sed 's/, $//') }"

# clean up
rm $object 
# rm $binary

echo $bytes
