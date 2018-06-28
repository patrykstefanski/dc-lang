#ifndef CODE_GEN_HPP
#define CODE_GEN_HPP

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Target/TargetMachine.h>

#include "instruction.hpp"

class CodeGen {
public:
  typedef std::uint64_t (*TracePtr)(std::int64_t*);

  static void init();

  CodeGen();

  // Compiles the trace and returns a pointer to the compiled code.
  TracePtr compile(const std::vector<Instruction>& bytecode,
                   const std::vector<std::int64_t>& constants,
                   const std::vector<std::uint32_t>& trace);

private:
  struct TraceInfo {
    std::uint64_t finish_ret;
    bool single_ret;
  };

  void optimize(llvm::Module* module);

  llvm::LLVMContext context_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> target_machine_;
  const llvm::DataLayout data_layout_;
  llvm::orc::RTDyldObjectLinkingLayer object_layer_;
  llvm::orc::IRCompileLayer<decltype(object_layer_), llvm::orc::SimpleCompiler>
      compile_layer_;
  std::unordered_map<std::uint64_t, TraceInfo> trace_info_;
};

#endif  // !CODE_GEN_HPP
