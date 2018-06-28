#include "instruction.hpp"

#include <cstdint>
#include <cstdio>

#include "cxx_extensions.hpp"

void Instruction::print(std::FILE* file) const {
  switch (opcode()) {
  // Const instruction.
  case Instruction::CONST:
    std::fprintf(file, "const %u, $%u", a(), static_cast<std::uint16_t>(d()));
    break;
  // Move instructions.
  case Instruction::MOVR:
    std::fprintf(file, "movr  %u, %u", a(), b());
    break;
  case Instruction::MOVI:
    std::fprintf(file, "movi  %u, $%i", a(), d());
    break;
  // Commutative binary instructions.
  case Instruction::ADDRR:
    std::fprintf(file, "addrr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::MULRR:
    std::fprintf(file, "mulrr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::EQRR:
    std::fprintf(file, "eqrr  %u, %u, %u", a(), b(), c());
    break;
  case Instruction::NERR:
    std::fprintf(file, "nerr  %u, %u, %u", a(), b(), c());
    break;
  case Instruction::ADDRI:
    std::fprintf(file, "addri %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::MULRI:
    std::fprintf(file, "mulri %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::EQRI:
    std::fprintf(file, "eqri  %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::NERI:
    std::fprintf(file, "neri  %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  // Noncommutative binary instructions.
  case Instruction::SUBRR:
    std::fprintf(file, "subrr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::DIVRR:
    std::fprintf(file, "divrr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::MODRR:
    std::fprintf(file, "modrr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::LTRR:
    std::fprintf(file, "ltrr  %u, %u, %u", a(), b(), c());
    break;
  case Instruction::LERR:
    std::fprintf(file, "lerr  %u, %u, %u", a(), b(), c());
    break;
  case Instruction::SUBRI:
    std::fprintf(file, "subri %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::DIVRI:
    std::fprintf(file, "divri %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::MODRI:
    std::fprintf(file, "modri %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::LTRI:
    std::fprintf(file, "ltri  %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::LERI:
    std::fprintf(file, "leri  %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::SUBIR:
    std::fprintf(file, "subir %u, $%i, %u", a(), static_cast<std::int8_t>(b()),
                 c());
    break;
  case Instruction::DIVIR:
    std::fprintf(file, "divir %u, $%i, %u", a(), static_cast<std::int8_t>(b()),
                 c());
    break;
  case Instruction::MODIR:
    std::fprintf(file, "modir %u, $%i, %u", a(), static_cast<std::int8_t>(b()),
                 c());
    break;
  case Instruction::LTIR:
    std::fprintf(file, "ltir  %u, $%i, %u", a(), static_cast<std::int8_t>(b()),
                 c());
    break;
  case Instruction::LEIR:
    std::fprintf(file, "leir  %u, $%i, %u", a(), static_cast<std::int8_t>(b()),
                 c());
    break;
  // Unary instructions.
  case Instruction::NEG:
    std::fprintf(file, "neg   %u, %u", a(), b());
    break;
  case Instruction::NOT:
    std::fprintf(file, "not   %u, %u", a(), b());
    break;
  // Array instructions.
  case Instruction::ANEWR:
    std::fprintf(file, "anewr %u, %u", a(), b());
    break;
  case Instruction::ANEWI:
    std::fprintf(file, "anewi %u, $%u", a(), static_cast<std::uint16_t>(d()));
    break;
  case Instruction::ADEL:
    std::fprintf(file, "adel  %u", a());
    break;
  case Instruction::AGETR:
    std::fprintf(file, "agetr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::AGETI:
    std::fprintf(file, "ageti %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  case Instruction::ASETR:
    std::fprintf(file, "asetr %u, %u, %u", a(), b(), c());
    break;
  case Instruction::ASETI:
    std::fprintf(file, "aseti %u, %u, $%i", a(), b(),
                 static_cast<std::int8_t>(c()));
    break;
  // Jump instructions.
  case Instruction::JMP:
    std::fprintf(file, "jmp   $%i", static_cast<std::int16_t>(d()));
    break;
  case Instruction::JT:
    std::fprintf(file, "jt    %u, $%i", a(), static_cast<std::int16_t>(d()));
    break;
  case Instruction::JF:
    std::fprintf(file, "jf    %u, $%i", a(), static_cast<std::int16_t>(d()));
    break;
  // Call/ret instructions.
  case Instruction::CALL:
    std::fprintf(file, "call  %u, %u", a(), b());
    break;
  case Instruction::RETR:
    std::fprintf(file, "retr  %u", a());
    break;
  case Instruction::RETI:
    std::fprintf(file, "reti  $%i", d());
    break;
  // Loop instruction.
  case Instruction::LOOP:
    std::fprintf(file, "loop");
    break;
  case Instruction::LOOPC:
    std::fprintf(file, "loopc");
    break;
  // System instructions.
  case Instruction::EXIT:
    std::fprintf(file, "exit  %u", a());
    break;
  case Instruction::IN:
    std::fprintf(file, "in    %u", a());
    break;
  case Instruction::OUT:
    std::fprintf(file, "out   %u", a());
    break;
  default:
    UNREACHABLE();
  }
}
