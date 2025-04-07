#ifndef RISCV_GEN
#define RISCV_GEN

#include "beam_defs.hpp"
#include "pcb.hpp"
#include <cassert>
#include <cstdint>
#include <cstring>
#include <format>
#include <stdexcept>
#include <vector>

struct RISCV_Instruction {
  uint8_t raw[4];

  RISCV_Instruction(uint32_t instr) { memcpy(this->raw, &instr, 4); }
  RISCV_Instruction() { memset(raw, 0, 4); }

  void set_opcode(uint8_t op_code) { set_bits(0, 7, op_code); }

  void set_rd(uint8_t rd) { set_bits(7, 12, rd); }

  void set_funct3(uint8_t funct3) { set_bits(12, 15, funct3); }

  void set_rs1(uint8_t rs1) { set_bits(15, 20, rs1); }

  void set_rs2(uint8_t rs2) { set_bits(20, 25, rs2); }

  void set_bits(uint8_t start, uint8_t stop, uint16_t value,
                uint8_t value_start = 0) {
    const auto size = stop - start;
    const auto mask = (1UL << size) - 1;
    const auto masked = (value >> value_start) & mask;

    uint32_t *p = reinterpret_cast<uint32_t *>(raw);
    *p &= ~(mask << start); // 0 out the bits
    *p |= masked << start;
  }

  std::string display_bits() {
    uint32_t *p = reinterpret_cast<uint32_t *>(raw);
    return std::format("{:08b}", *p);
  }

  std::string display_hex() {
    std::string out;

    for (int i = 0; i < 4; i++) {
      out += std::format("0x{:02x}, ", raw[i]);
    }

    return out;
  }
};

inline void set_imm_B_type_instruction(RISCV_Instruction &instr, int16_t imm) {
  assert(-4096 < imm && imm < 4096);
  assert((imm & 0b1) == 0); // last bit is ignored

  instr.set_bits(8, 12, imm, 1);
  instr.set_bits(25, 31, imm, 5);
  instr.set_bits(7, 8, imm, 11);
  instr.set_bits(31, 32, imm, 12);
}

inline RISCV_Instruction create_B_type_instruction(uint8_t opcode,
                                                   uint8_t funct3, uint8_t rs1,
                                                   uint8_t rs2, int16_t imm) {
  assert(0 <= rs1 && rs1 < 32);
  assert(0 <= rs2 && rs2 < 32);

  RISCV_Instruction out;
  out.set_opcode(opcode);
  out.set_funct3(funct3);
  out.set_rs1(rs1);
  out.set_rs2(rs2);

  set_imm_B_type_instruction(out, imm);

  return out;
}

inline RISCV_Instruction create_I_type_instruction(uint8_t opcode, uint8_t rd,
                                                   uint8_t funct3, uint8_t rs1,
                                                   int16_t imm) {
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rd && rd < 32);
  assert(0 <= rs1 && rs1 < 32);

  RISCV_Instruction out;
  out.set_opcode(opcode);
  out.set_funct3(funct3);
  out.set_rs1(rs1);
  out.set_rd(rd);

  out.set_bits(20, 32, imm, 0);

  return out;
}

inline RISCV_Instruction create_S_type_instruction(uint8_t opcode,
                                                   uint8_t funct3, uint8_t rs1,
                                                   uint8_t rs2, int16_t imm) {
  assert(-2047 < imm && imm < 2048);
  assert(0 <= rs1 && rs1 < 32);
  assert(0 <= rs2 && rs2 < 32);

  RISCV_Instruction out;
  out.set_opcode(opcode);
  out.set_funct3(funct3);
  out.set_rs1(rs1);
  out.set_rs2(rs2);

  out.set_bits(7, 12, imm, 0);
  out.set_bits(25, 32, imm, 5);

  return out;
}

inline RISCV_Instruction create_branch_equal(uint8_t rs1, uint8_t rs2,
                                             uint16_t imm) {
  constexpr auto op_code_bits = 0b1100011;
  constexpr auto funct3_bits = 0x0;

  return create_B_type_instruction(op_code_bits, funct3_bits, rs1, rs2, imm);
}

inline RISCV_Instruction create_branch_not_equal(uint8_t rs1, uint8_t rs2,
                                                 uint16_t imm) {
  constexpr auto op_code_bits = 0b1100011;
  constexpr auto funct3_bits = 0x1;

  return create_B_type_instruction(op_code_bits, funct3_bits, rs1, rs2, imm);
}

inline RISCV_Instruction create_add_immediate(uint8_t rd, uint8_t rs,
                                              uint16_t imm) {
  constexpr auto op_code_bits = 0b0010011; // load
  constexpr auto funct3_bits = 0x0;
  //
  return create_I_type_instruction(op_code_bits, rd, funct3_bits, rs, imm);
}

inline RISCV_Instruction create_load_doubleword(uint8_t rd, uint8_t rs,
                                                int16_t imm) {
  constexpr auto load_instr_bits = 0b0000011; // load
  constexpr auto funct3_bits = 0x3;           // width

  return create_I_type_instruction(load_instr_bits, rd, funct3_bits, rs, imm);
}

inline RISCV_Instruction create_store_doubleword(uint8_t rs1, uint8_t rs2,
                                                 int16_t imm) {
  constexpr auto store_instr_bits = 0b0100011; // store
  constexpr auto funct3_bits = 0x3;            // width

  return create_S_type_instruction(store_instr_bits, funct3_bits, rs1, rs2,
                                   imm);
}

inline RISCV_Instruction create_load_x_reg(uint8_t riscv_dest_reg,
                                           int16_t x_reg_num,
                                           uint8_t x_array_register) {

  // ld riscv_dest_reg, x_reg_num(s5)
  return create_load_doubleword(riscv_dest_reg, x_array_register,
                                x_reg_num * 8);
}

inline std::vector<RISCV_Instruction>
create_load_y_reg(uint8_t riscv_dest_reg, uint16_t y_reg_num,
                  uint8_t pcb_p_register) {

  return std::vector<RISCV_Instruction>{
      // ld riscv_dest_reg, STOP_index(pcb_p_register)
      create_load_doubleword(riscv_dest_reg, pcb_p_register, STOP * 8),

      // ld riscv_dest_reg, y_reg_num(riscv_dest_reg)
      create_store_doubleword(riscv_dest_reg, riscv_dest_reg, y_reg_num * 8)};
}

inline RISCV_Instruction create_store_x_reg(uint8_t riscv_dest_reg,
                                            uint16_t x_reg_num,
                                            uint8_t x_array_register) {
  // sd riscv_dest_reg, x_reg_num(s5)
  return create_store_doubleword(x_array_register, riscv_dest_reg,
                                 x_reg_num * 8);
}

inline std::vector<RISCV_Instruction>
create_store_y_reg(uint8_t riscv_dest_reg, uint16_t y_reg_num,
                   uint8_t pcb_p_register) {

  // either t0 or t1, making sure there is no clash
  auto other_register = riscv_dest_reg == 5 ? 6 : 5;

  return std::vector<RISCV_Instruction>{
      // ld other_register, STOP_index(pcb_p_register)
      create_load_doubleword(other_register, pcb_p_register, STOP * 8),

      // +1 because the code_pointer is allocated there
      // sd riscv_dest_reg, y_reg_num + 1(other_register)
      create_store_doubleword(other_register, riscv_dest_reg,
                              (y_reg_num + 1) * 8)};
}

inline std::vector<RISCV_Instruction>
create_load_appropriate(Argument arg, uint8_t dest_reg) {
  switch (arg.tag) {
  case X_REGISTER_TAG: {
    // assume s5 is the x array register
    return std::vector<RISCV_Instruction>{
        create_load_x_reg(dest_reg, arg.arg_raw.arg_num, 21)};
  }
  case Y_REGISTER_TAG: {
    // assumes s1 points to the pcb
    return create_load_y_reg(dest_reg, arg.arg_raw.arg_num, 9);
  }
  case LITERAL_TAG: {
    // TODO I am unsure if the 'LITERAL' tag is what I think it is
    // return an empty vector as we don't need to make any changes
    return std::vector<RISCV_Instruction>();
  }
  default:
    throw std::logic_error(
        std::format("Cannot load the tag {}", TagToString(arg.tag)));
  }
}

inline std::vector<RISCV_Instruction>
create_store_appropriate(Argument arg, uint8_t src_reg) {
  switch (arg.tag) {
  case X_REGISTER_TAG: {
    // assume s5 is the x array register
    return std::vector<RISCV_Instruction>{
        create_store_x_reg(src_reg, arg.arg_raw.arg_num, 21)};
  }
  case Y_REGISTER_TAG: {
    // assumes s1 points to the pcb
    return create_store_y_reg(src_reg, arg.arg_raw.arg_num, 9);
  }
  default:
    throw std::logic_error(
        std::format("Cannot store at this tag", TagToString(arg.tag)));
  }
}

#endif
