assembly="$1"
name="${assembly%.S}"
object="$name.o"
binary="$name.bin"

riscv64-linux-gnu-as -o $object $assembly
riscv64-linux-gnu-objcopy -O binary $object $binary
bytes="uint8_t code[] = { $(xxd -p $binary | sed 's/\(..\)/0x\1,/g' | sed 's/,$//') };"
echo $bytes
