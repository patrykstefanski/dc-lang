#ifndef INTERPRETER_HPP
#define INTERPRETER_HPP

#include <cstdint>
#include <vector>

#include "instruction.hpp"

// Interprets the bytecode, returns the exit code of the interpreted program.
int interpret(std::vector<Instruction> bytecode,
              std::vector<std::int64_t> constants);

#endif  // !INTERPRETER_HPP
