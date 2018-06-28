#ifndef INSTRUCTION_HPP
#define INSTRUCTION_HPP

#include <cstdint>
#include <cstdio>

#include "cxx_extensions.hpp"

class Instruction {
public:
  enum Opcode : std::uint8_t {
    // Const instruction.
    CONST,  // a <- constants[$d]
    // Move instructions.
    MOVR,  // a <- b
    MOVI,  // a <- $d
    // Commutative binary instructions.
    ADDRR,  // a <- b + c
    MULRR,  // a <- b * c
    EQRR,   // a <- b == c
    NERR,   // a <- b != c
    ADDRI,  // a <- b + $c
    MULRI,  // a <- b * $c
    EQRI,   // a <- b == $c
    NERI,   // a <- b != $c
    // Noncommutative binary instructions.
    SUBRR,  // a <- b - c
    DIVRR,  // a <- b / c
    MODRR,  // a <- b % c
    LTRR,   // a <- b < c
    LERR,   // a <- b <= c
    SUBRI,  // a <- b - $c
    DIVRI,  // a <- b / $c
    MODRI,  // a <- b % $c
    LTRI,   // a <- b < $c
    LERI,   // a <- b <= $c
    SUBIR,  // a <- $b - c
    DIVIR,  // a <- $b / c
    MODIR,  // a <- $b % c
    LTIR,   // a <- $b < c
    LEIR,   // a <- $b <= c
    // Unary instructions.
    NEG,  // a <- -b
    NOT,  // a <- !b
    // Array instructions.
    ANEWR,  // a <- new int64_t[b]
    ANEWI,  // a <- new int64_t[$d]
    ADEL,   // delete[] a
    AGETR,  // a <- b[c]
    AGETI,  // a <- b[$c]
    ASETR,  // b[c] <- a
    ASETI,  // b[$c] <- a
    // Jump instructions.
    JMP,  // goto d
    JT,   // if a != 0 goto $d
    JF,   // if a == 0 goto $d
    // Call/ret instructions.
    CALL,  // a <- a(a + 1, a + 2, ..., a + b)
    RETR,  // return a
    RETI,  // return $d
    // Loop instruction.
    LOOP,   // Loop header
    LOOPC,  // JIT compiled loop
    // System instructions.
    EXIT,  // exit(a)
    IN,    // scanf("%" PRId64, &a);
    OUT,   // printf("%" PRId64 "\n", a);
  };

  // Make a NOP instruction.
  Instruction() : opcode_(ADDRI), a_(0), bc_({0, 0}) {
  }

  Instruction(const Instruction&) = default;
  Instruction& operator=(const Instruction&) = default;

  static Instruction make_abc(Opcode opcode, std::uint8_t a, std::uint8_t b,
                              std::uint8_t c) {
    Instruction instruction;
    instruction.opcode_ = opcode;
    instruction.a_ = a;
    instruction.bc_.b_ = b;
    instruction.bc_.c_ = c;
    return instruction;
  }

  static Instruction make_ad(Opcode opcode, std::uint8_t a, std::uint16_t d) {
    Instruction instruction;
    instruction.opcode_ = opcode;
    instruction.a_ = a;
    instruction.d_ = d;
    return instruction;
  }

  // Prints the instruction.
  COLD void print(std::FILE* file) const;

  Opcode opcode() const {
    return opcode_;
  }

  void set_opcode(Opcode opcode) {
    opcode_ = opcode;
  }

  std::uint8_t a() const {
    return a_;
  }

  void set_a(std::uint8_t a) {
    a_ = a;
  }

  std::uint8_t b() const {
    return bc_.b_;
  }

  void set_b(std::uint8_t b) {
    bc_.b_ = b;
  }

  std::uint8_t c() const {
    return bc_.c_;
  }

  void set_c(std::uint8_t c) {
    bc_.c_ = c;
  }

  std::int16_t d() const {
    return d_;
  }

  void set_d(std::int16_t d) {
    d_ = d;
  }

private:
  Opcode opcode_;
  std::uint8_t a_;
  union {
    struct {
      std::uint8_t b_;
      std::uint8_t c_;
    } bc_;
    std::int16_t d_;
  };
};

static_assert(sizeof(Instruction) == sizeof(std::uint32_t));

#endif  // !INSTRUCTION_HPP
