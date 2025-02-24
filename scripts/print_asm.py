from capstone import Cs, CS_ARCH_RISCV, CS_MODE_RISCV64

raw_code = input("Bytes: ")
code_bytes = bytes(
        [int(hex_byte, 16) for hex_byte in raw_code.strip(", ").split(", ")]
)

md = Cs(CS_ARCH_RISCV, CS_MODE_RISCV64)
base_addr = 0x80000000

for i in md.disasm(code_bytes, base_addr):
    print("0x%x:\t%s\t%s" % (i.address, i.mnemonic, i.op_str))
