#include "interpreter.hpp"

#include <algorithm>
#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <unordered_map>
#include <vector>

#include "assert.hpp"
#include "code_gen.hpp"
#include "cxx_extensions.hpp"
#include "instruction.hpp"

constexpr std::int32_t hot_threshold = 8;
constexpr std::uint32_t trace_capacity = 1024;
constexpr std::uint32_t memory_size = 128 * 1024;

extern "C" std::int64_t _in(void) {
  std::int64_t value;
  // Suppress the unused result warning.
  if (std::scanf("%" PRId64, &value)) {
  }
  return value;
}

extern "C" void _out(std::int64_t value) {
  std::printf("%" PRId64 "\n", value);
}

extern int jit_flag;

int interpret(std::vector<Instruction> bytecode,
              std::vector<std::int64_t> constants) {
  std::unordered_map<Instruction*, std::int32_t> loop_hits;
  std::vector<std::uint32_t> trace;
  trace.reserve(trace_capacity);

  CodeGen code_gen;

  std::vector<std::int64_t> memory(memory_size / sizeof(std::int64_t));
  std::int64_t* regs = memory.data();
  auto* ip = bytecode.data();
  const std::int64_t* consts = constants.data();

  static const void* const static_dispatch_table[] = {
      // Const instruction.
      [Instruction::CONST] = &&const_,
      // Move instructions.
      [Instruction::MOVR] = &&movr,
      [Instruction::MOVI] = &&movi,
      // Commutative binary instructions.
      [Instruction::ADDRR] = &&addrr,
      [Instruction::MULRR] = &&mulrr,
      [Instruction::EQRR] = &&eqrr,
      [Instruction::NERR] = &&nerr,
      [Instruction::ADDRI] = &&addri,
      [Instruction::MULRI] = &&mulri,
      [Instruction::EQRI] = &&eqri,
      [Instruction::NERI] = &&neri,
      // Noncommutative binary instructions.
      [Instruction::SUBRR] = &&subrr,
      [Instruction::DIVRR] = &&divrr,
      [Instruction::MODRR] = &&modrr,
      [Instruction::LTRR] = &&ltrr,
      [Instruction::LERR] = &&lerr,
      [Instruction::SUBRI] = &&subri,
      [Instruction::DIVRI] = &&divri,
      [Instruction::MODRI] = &&modri,
      [Instruction::LTRI] = &&ltri,
      [Instruction::LERI] = &&leri,
      [Instruction::SUBIR] = &&subir,
      [Instruction::DIVIR] = &&divir,
      [Instruction::MODIR] = &&modir,
      [Instruction::LTIR] = &&ltir,
      [Instruction::LEIR] = &&leir,
      // Unary instructions.
      [Instruction::NEG] = &&neg,
      [Instruction::NOT] = &&not_,
      // Array instructions.
      [Instruction::ANEWR] = &&anewr,
      [Instruction::ANEWI] = &&anewi,
      [Instruction::ADEL] = &&adel,
      [Instruction::AGETR] = &&agetr,
      [Instruction::AGETI] = &&ageti,
      [Instruction::ASETR] = &&asetr,
      [Instruction::ASETI] = &&aseti,
      // Jump instructions.
      [Instruction::JMP] = &&jmp,
      [Instruction::JT] = &&jt,
      [Instruction::JF] = &&jf,
      // Call/ret instructions.
      [Instruction::CALL] = &&call,
      [Instruction::RETR] = &&retr,
      [Instruction::RETI] = &&reti,
      // Loop instruction.
      [Instruction::LOOP] = jit_flag ? &&jit_loop : &&loop,
      [Instruction::LOOPC] = jit_flag ? &&jit_loopc : nullptr,
      // System instructions.
      [Instruction::EXIT] = &&exit,
      [Instruction::IN] = &&in,
      [Instruction::OUT] = &&out,
  };
  constexpr std::size_t dispatch_table_size =
      sizeof(static_dispatch_table) / sizeof(*static_dispatch_table);
  const void* dynamic_dispatch_table[dispatch_table_size];
  std::copy_n(static_dispatch_table, dispatch_table_size,
              dynamic_dispatch_table);

#define NEXT goto* dynamic_dispatch_table[ip->opcode()]

  NEXT;

  // Const instruction.

const_ : {
  auto index = static_cast<std::uint16_t>(ip->d());
  regs[ip->a()] = consts[index];
  ++ip;
  NEXT;
}

  // Move instructions.

movr : {
  regs[ip->a()] = regs[ip->b()];
  ++ip;
  NEXT;
}

movi : {
  regs[ip->a()] = ip->d();
  ++ip;
  NEXT;
}

  // Commutative binary instructions.

addrr : {
  regs[ip->a()] = regs[ip->b()] + regs[ip->c()];
  ++ip;
  NEXT;
}

mulrr : {
  regs[ip->a()] = regs[ip->b()] * regs[ip->c()];
  ++ip;
  NEXT;
}

eqrr : {
  regs[ip->a()] = regs[ip->b()] == regs[ip->c()];
  ++ip;
  NEXT;
}

nerr : {
  regs[ip->a()] = regs[ip->b()] != regs[ip->c()];
  ++ip;
  NEXT;
}

addri : {
  regs[ip->a()] = regs[ip->b()] + static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

mulri : {
  regs[ip->a()] = regs[ip->b()] * static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

eqri : {
  regs[ip->a()] = regs[ip->b()] == static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

neri : {
  regs[ip->a()] = regs[ip->b()] != static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

  // Noncommutative binary instructions.

subrr : {
  regs[ip->a()] = regs[ip->b()] - regs[ip->c()];
  ++ip;
  NEXT;
}

divrr : {
  regs[ip->a()] = regs[ip->b()] / regs[ip->c()];
  ++ip;
  NEXT;
}

modrr : {
  regs[ip->a()] = regs[ip->b()] % regs[ip->c()];
  ++ip;
  NEXT;
}

ltrr : {
  regs[ip->a()] = regs[ip->b()] < regs[ip->c()];
  ++ip;
  NEXT;
}

lerr : {
  regs[ip->a()] = regs[ip->b()] <= regs[ip->c()];
  ++ip;
  NEXT;
}

subri : {
  regs[ip->a()] = regs[ip->b()] - static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

divri : {
  regs[ip->a()] = regs[ip->b()] / static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

modri : {
  regs[ip->a()] = regs[ip->b()] % static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

ltri : {
  regs[ip->a()] = regs[ip->b()] < static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

leri : {
  regs[ip->a()] = regs[ip->b()] <= static_cast<std::int8_t>(ip->c());
  ++ip;
  NEXT;
}

subir : {
  regs[ip->a()] = static_cast<std::int8_t>(ip->b()) - regs[ip->c()];
  ++ip;
  NEXT;
}

divir : {
  regs[ip->a()] = static_cast<std::int8_t>(ip->b()) / regs[ip->c()];
  ++ip;
  NEXT;
}

modir : {
  regs[ip->a()] = static_cast<std::int8_t>(ip->b()) % regs[ip->c()];
  ++ip;
  NEXT;
}

ltir : {
  regs[ip->a()] = static_cast<std::int8_t>(ip->b()) < regs[ip->c()];
  ++ip;
  NEXT;
}

leir : {
  regs[ip->a()] = static_cast<std::int8_t>(ip->b()) <= regs[ip->c()];
  ++ip;
  NEXT;
}

  // Unary instructions.

neg : {
  regs[ip->a()] = -regs[ip->b()];
  ++ip;
  NEXT;
}

not_ : {
  regs[ip->a()] = !regs[ip->b()];
  ++ip;
  NEXT;
}

  // Array instructions.

anewr : {
  auto n = regs[ip->b()];
  regs[ip->a()] =
      reinterpret_cast<std::int64_t>(std::malloc(n * sizeof(std::int64_t)));
  ++ip;
  NEXT;
}

anewi : {
  auto n = static_cast<std::uint16_t>(ip->d());
  regs[ip->a()] =
      reinterpret_cast<std::int64_t>(std::malloc(n * sizeof(std::int64_t)));
  ++ip;
  NEXT;
}

adel : {
  std::free(reinterpret_cast<void*>(regs[ip->a()]));
  ++ip;
  NEXT;
}

agetr : {
  const std::int64_t* array = reinterpret_cast<std::int64_t*>(regs[ip->b()]);
  std::int64_t index = regs[ip->c()];
  regs[ip->a()] = array[index];
  ++ip;
  NEXT;
}

ageti : {
  const std::int64_t* array = reinterpret_cast<std::int64_t*>(regs[ip->b()]);
  std::int64_t index = static_cast<std::int8_t>(ip->c());
  regs[ip->a()] = array[index];
  ++ip;
  NEXT;
}

asetr : {
  std::int64_t* array = reinterpret_cast<std::int64_t*>(regs[ip->b()]);
  std::int64_t index = regs[ip->c()];
  array[index] = regs[ip->a()];
  ++ip;
  NEXT;
}

aseti : {
  std::int64_t* array = reinterpret_cast<std::int64_t*>(regs[ip->b()]);
  std::int64_t index = static_cast<std::int8_t>(ip->c());
  array[index] = regs[ip->a()];
  ++ip;
  NEXT;
}

  // Jump instructions.

jmp : {
  ip += ip->d() + 1;
  NEXT;
}

jt : {
  if (regs[ip->a()] != 0) {
    ip += ip->d() + 1;
  } else {
    ++ip;
  }
  NEXT;
}

jf : {
  if (regs[ip->a()] == 0) {
    ip += ip->d() + 1;
  } else {
    ++ip;
  }
  NEXT;
}

  // Call/ret instructions.

call : {
  std::int32_t a = ip->a();
  const auto* tmp = ip;
  ip += regs[a] + 1;
  regs[a] = reinterpret_cast<std::int64_t>(tmp);
  regs += a + 1;
  NEXT;
}

retr : {
  std::int64_t ret = regs[ip->a()];
  ip = reinterpret_cast<Instruction*>(regs[-1]);
  regs[-1] = ret;
  regs -= ip->a() + 1;
  ++ip;
  NEXT;
}

reti : {
  std::int64_t ret = ip->d();
  ip = reinterpret_cast<Instruction*>(regs[-1]);
  regs[-1] = ret;
  regs -= ip->a() + 1;
  ++ip;
  NEXT;
}

  // Loop instruction.

loop : {
  ++ip;
  NEXT;
}

  // System instructions.

exit : { return regs[ip->a()]; }

in : {
  regs[ip->a()] = _in();
  ++ip;
  NEXT;
}

out : {
  _out(regs[ip->a()]);
  ++ip;
  NEXT;
}

  // JIT stuff.

jit_loop : {
  std::int32_t hits = loop_hits[ip]++;
  if (UNLIKELY(hits >= hot_threshold)) {
    std::fill_n(dynamic_dispatch_table, dispatch_table_size, &&jit_trace);
    dynamic_dispatch_table[Instruction::JMP] = &&jit_jmp;
    dynamic_dispatch_table[Instruction::LOOP] = &&jit_inner_loop;
  }
  ++ip;
  NEXT;
}

jit_loopc : {
  // Execute compiled trace.
  auto ptr = reinterpret_cast<CodeGen::TracePtr>(constants[ip->d()]);
  std::uint64_t ret = ptr(regs);

  std::uint64_t pc_offset = ret & 0xffffffff;
  ip = bytecode.data() + pc_offset;

  std::uint64_t regs_offset = ret >> 32;
  regs += regs_offset;

  NEXT;
}

jit_trace : {
  if (LIKELY(trace.size() < trace_capacity)) {
    trace.push_back(ip - bytecode.data());
  } else {
    // Mark this loop as very cold.
    Instruction* loop = &bytecode[trace[0] - 1];
    ASSERT_EQ(loop->opcode(), Instruction::LOOP);
    loop_hits[loop] = std::numeric_limits<std::int32_t>::min();

    // Exit tracing.
    trace.clear();
    std::copy_n(static_dispatch_table, dispatch_table_size,
                dynamic_dispatch_table);
  }
  goto* static_dispatch_table[ip->opcode()];
}

jit_jmp : {
  if (ip->d() < 0) {
    ip += ip->d() + 1;
    ASSERT_EQ(ip->opcode(), Instruction::LOOP);

    // Compile trace.
    auto ptr = code_gen.compile(bytecode, constants, trace);
    std::size_t index = constants.size();
    constants.push_back(reinterpret_cast<std::int64_t>(ptr));
    ASSERT_LE(index, std::numeric_limits<std::uint16_t>::max());
    std::uint16_t d = index;

    // Replace with LOOPC.
    *ip = Instruction::make_ad(Instruction::LOOPC, 0, d);

    // Exit tracing.
    trace.clear();
    std::copy_n(static_dispatch_table, dispatch_table_size,
                dynamic_dispatch_table);

    goto jit_loopc;
  }
  goto jit_trace;
}

jit_inner_loop : {
  // Reset the loop hits to avoid entering the tracing mode too often.
  Instruction* loop = &bytecode[trace[0] - 1];
  ASSERT_EQ(loop->opcode(), Instruction::LOOP);
  loop_hits[loop] = 0;

  // Abort tracing, as any inner loop must be compiled first.
  trace.clear();
  std::copy_n(static_dispatch_table, dispatch_table_size,
              dynamic_dispatch_table);

  ASSERT_EQ(ip->opcode(), Instruction::LOOP);
  goto jit_loop;
}

  UNREACHABLE();
}
