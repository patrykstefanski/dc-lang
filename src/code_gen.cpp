#include "code_gen.hpp"

#include <cinttypes>
#include <cstdint>
#include <cstdio>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <utility>
#include <vector>

#include <capstone.h>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/RuntimeDyld.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar.h>

#include "assert.hpp"
#include "cxx_extensions.hpp"
#include "instruction.hpp"

extern int debug_flag;

void CodeGen::init() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
}

CodeGen::CodeGen()
  : builder_(context_),
    target_machine_(llvm::EngineBuilder().selectTarget()),
    data_layout_(target_machine_->createDataLayout()),
    object_layer_(
        []() {
          return std::make_shared<llvm::SectionMemoryManager>();
        },
        debug_flag == 0 ?
            decltype(object_layer_)::NotifyLoadedFtor() :
            [](decltype(object_layer_)::ObjHandleT,
               const decltype(object_layer_)::ObjectPtr& ptr,
               const llvm::RuntimeDyld::LoadedObjectInfo&) {
              std::fputs(
                  "\n"
                  "---------\n"
                  "Assembly:\n"
                  "---------\n",
                  stderr);
              auto sizes = llvm::object::computeSymbolSizes(*ptr->getBinary());
              for (const auto& [symbol, size] : sizes) {
                if (size == 0) {
                  continue;
                }
                auto s = symbol.getSection();
                if (!s) {
                  std::fputs("Failed to get section\n", stderr);
                  continue;
                }
                auto section = **s; // TODO
                llvm::StringRef contents;
                section.getContents(contents);
                csh handle;
                if (cs_open(CS_ARCH_X86, CS_MODE_64, &handle) != CS_ERR_OK) {
                  std::fputs("Failed to init capstone\n", stderr);
                  continue;
                }
                cs_insn *insts;
                auto data = reinterpret_cast<const std::uint8_t*>(
                    contents.data());
                auto count = cs_disasm(handle, data, contents.size(), 0, 0,
                                       &insts);
                for (auto inst = insts; inst < insts + count; ++inst) {
                  std::fprintf(stderr, "0x%04" PRIx64 ":\t%s\t\t%s\n",
                               inst->address, inst->mnemonic, inst->op_str);
                }
                cs_free(insts, count);
                cs_close(&handle);
              }
            }
    ),
    compile_layer_(object_layer_,
                   llvm::orc::SimpleCompiler(*target_machine_)) {
  llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
}

CodeGen::TracePtr CodeGen::compile(const std::vector<Instruction>& bytecode,
                                   const std::vector<std::int64_t>& constants,
                                   const std::vector<std::uint32_t>& trace) {
  auto module = std::make_unique<llvm::Module>("dc-lang", context_);
  module->setDataLayout(target_machine_->createDataLayout());

  // Some functions we may need to link to.
  auto malloc_fn = llvm::Function::Create(
      llvm::FunctionType::get(
          llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(context_)),
          {llvm::Type::getInt64Ty(context_)}, false),
      llvm::Function::ExternalLinkage, "malloc", module.get());
  auto free_fn = llvm::Function::Create(
      llvm::FunctionType::get(
          llvm::Type::getVoidTy(context_),
          {llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(context_))},
          false),
      llvm::Function::ExternalLinkage, "free", module.get());
  auto exit_fn = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getVoidTy(context_),
                              {llvm::Type::getInt64Ty(context_)}, false),
      llvm::Function::ExternalLinkage, "exit", module.get());
  auto in_fn = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getInt64Ty(context_), {}, false),
      llvm::Function::ExternalLinkage, "_in", module.get());
  auto out_fn = llvm::Function::Create(
      llvm::FunctionType::get(llvm::Type::getVoidTy(context_),
                              {llvm::Type::getInt64Ty(context_)}, false),
      llvm::Function::ExternalLinkage, "_out", module.get());

  // A function for the compiled trace.
  auto trace_fn = llvm::Function::Create(
      llvm::FunctionType::get(
          llvm::Type::getInt64Ty(context_),
          {llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(context_))},
          false),
      llvm::Function::ExternalLinkage, "trace", module.get());
  auto arg = trace_fn->args().begin();

  // A block for allocas and for loading registers from memory, which are
  // passed as an array to the compiled function.
  auto prologue = llvm::BasicBlock::Create(context_, "prologue", trace_fn);
  builder_.SetInsertPoint(prologue);
  auto ret = builder_.CreateAlloca(llvm::Type::getInt64Ty(context_), 0, nullptr,
                                   "ret");

  // A block for storing registers back to memory after exiting from the loop.
  auto epilogue = llvm::BasicBlock::Create(context_, "epilogue");

  // Additional blocks that require loading/storing registers, which will be
  // fixed after we know which registers are loaded/stored in the trace. This
  // blocks are required for calling nested traces. If a nested trace is called,
  // we have to store registers back to memory and restore them after the call.
  // Each pair consists of a block where the loads/stores following a branch
  // should be put into and a block that is the destination of the branch.
  std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> load_blocks;
  std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> store_blocks;

  // Properties of the compiled trace, which are useful for optimizations.
  // Generally, if a nested trace returns, the returned value must be checked
  // to see whether the nested trace was executed successfully, or some guard
  // failed (then, the outer trace must exit as well). If there is only a
  // a single return (only one guard for the loop condition), this check can
  // be optimized away.
  std::optional<std::uint64_t> finish_ret;
  bool single_ret = true;

  // Registers handling.
  // A mapping to alloca instructions stored in the prologue block.
  std::map<std::uint32_t, llvm::AllocaInst*> regs;
  // Sets of registers that are loaded/stored in the trace.
  std::set<std::uint32_t> loaded_regs;
  std::set<std::uint32_t> stored_regs;
  // If a call is made, used registers within that call must correspond to
  // different memory locations. For each call or ret instruction, an offset
  // is pushed or popped respectively.
  std::vector<std::uint32_t> reg_offsets{0};
  // Loading/storing utilities.
  auto load_reg = [&](std::int32_t reg) -> llvm::Value* {
    std::uint32_t regs_offset = reg_offsets.back();
    ASSERT_GE(reg + static_cast<std::int32_t>(regs_offset), 0);
    std::uint32_t r = reg + regs_offset;
    loaded_regs.insert(r);
    auto it = regs.find(r);
    if (it == regs.end()) {
      // Generate alloca and load the content from memory.
      auto insert_block = builder_.GetInsertBlock();
      builder_.SetInsertPoint(prologue);
      auto index = llvm::ConstantInt::get(context_, llvm::APInt(32, r));
      auto value = builder_.CreateLoad(builder_.CreateGEP(arg, index));
      auto name = llvm::Twine("r") + llvm::Twine(r);
      auto alloca = builder_.CreateAlloca(llvm::Type::getInt64Ty(context_), 0,
                                          nullptr, std::move(name));
      builder_.CreateStore(value, alloca);
      builder_.SetInsertPoint(insert_block);
      it = regs.emplace(r, alloca).first;
    }
    return builder_.CreateLoad(it->second);
  };
  auto store_reg = [&](std::int32_t reg, llvm::Value* value) {
    std::uint32_t regs_offset = reg_offsets.back();
    ASSERT_GE(reg + static_cast<std::int32_t>(regs_offset), 0);
    std::uint32_t r = reg + regs_offset;
    stored_regs.insert(r);
    auto it = regs.find(r);
    if (it == regs.end()) {
      // Generate alloca.
      auto insert_block = builder_.GetInsertBlock();
      builder_.SetInsertPoint(prologue);
      auto name = llvm::Twine("r") + llvm::Twine(r);
      auto alloca = builder_.CreateAlloca(llvm::Type::getInt64Ty(context_), 0,
                                          nullptr, std::move(name));
      builder_.SetInsertPoint(insert_block);
      it = regs.emplace(r, alloca).first;
    }
    builder_.CreateStore(value, it->second);
  };

  // Emit code for the loop.
  auto loop = llvm::BasicBlock::Create(context_, "loop", trace_fn);
  builder_.SetInsertPoint(loop);
  ASSERT_LE(trace.size(), std::numeric_limits<std::uint32_t>::max());
  for (std::uint32_t i = 0; i < trace.size(); ++i) {
    const auto pc = trace[i];
    const auto inst = bytecode[pc];
    const auto o = inst.opcode();
    if (o == Instruction::CONST) {
      std::uint16_t index = inst.d();
      auto val = static_cast<std::uint64_t>(constants[index]);
      auto res = llvm::ConstantInt::get(context_, llvm::APInt(64, val, true));
      store_reg(inst.a(), res);
    } else if (o == Instruction::MOVR) {
      auto res = load_reg(inst.b());
      store_reg(inst.a(), res);
    } else if (o == Instruction::MOVI) {
      auto val = static_cast<std::uint64_t>(inst.d());
      auto res = llvm::ConstantInt::get(context_, llvm::APInt(64, val, true));
      store_reg(inst.a(), res);
    } else if (o >= Instruction::ADDRR && o <= Instruction::LEIR) {
      // Binary op.
      llvm::Value* lhs;
      if (o >= Instruction::SUBIR && o <= Instruction::LEIR) {
        lhs = llvm::ConstantInt::get(context_, llvm::APInt(64, inst.b(), true));
      } else {
        lhs = load_reg(inst.b());
      }
      llvm::Value* rhs;
      if ((o >= Instruction::ADDRI && o <= Instruction::NERI) ||
          (o >= Instruction::SUBRI && o <= Instruction::LERI)) {
        rhs = llvm::ConstantInt::get(context_, llvm::APInt(64, inst.c(), true));
      } else {
        rhs = load_reg(inst.c());
      }
      llvm::Value* cond;
      llvm::Value* res;
      switch (o) {
      case Instruction::ADDRR:
      case Instruction::ADDRI:
        res = builder_.CreateAdd(lhs, rhs);
        break;
      case Instruction::MULRR:
      case Instruction::MULRI:
        res = builder_.CreateMul(lhs, rhs);
        break;
      case Instruction::EQRR:
      case Instruction::EQRI:
        cond = builder_.CreateICmpEQ(lhs, rhs);
        res = builder_.CreateIntCast(cond, llvm::Type::getInt64Ty(context_),
                                     true);
        break;
      case Instruction::NERR:
      case Instruction::NERI:
        cond = builder_.CreateICmpNE(lhs, rhs);
        res = builder_.CreateIntCast(cond, llvm::Type::getInt64Ty(context_),
                                     true);
        break;
      case Instruction::SUBRR:
      case Instruction::SUBRI:
      case Instruction::SUBIR:
        res = builder_.CreateSub(lhs, rhs);
        break;
      case Instruction::DIVRR:
      case Instruction::DIVRI:
      case Instruction::DIVIR:
        res = builder_.CreateSDiv(lhs, rhs);
        break;
      case Instruction::MODRR:
      case Instruction::MODRI:
      case Instruction::MODIR:
        res = builder_.CreateSRem(lhs, rhs);
        break;
      case Instruction::LTRR:
      case Instruction::LTRI:
      case Instruction::LTIR:
        cond = builder_.CreateICmpSLT(lhs, rhs);
        res = builder_.CreateIntCast(cond, llvm::Type::getInt64Ty(context_),
                                     true);
        break;
      case Instruction::LERR:
      case Instruction::LERI:
      case Instruction::LEIR:
        cond = builder_.CreateICmpSLE(lhs, rhs);
        res = builder_.CreateIntCast(cond, llvm::Type::getInt64Ty(context_),
                                     true);
        break;
      default:
        UNREACHABLE();
      }
      store_reg(inst.a(), res);
    } else if (o == Instruction::NEG || o == Instruction::NOT) {
      auto val = load_reg(inst.b());
      llvm::Value* res;
      if (o == Instruction::NEG) {
        res = builder_.CreateNeg(val);
      } else {
        res = builder_.CreateNot(val);
      }
      store_reg(inst.a(), res);
    } else if (o == Instruction::ANEWR || o == Instruction::ANEWI) {
      llvm::Value* size;
      if (o == Instruction::ANEWR) {
        auto n = load_reg(inst.b());
        auto s = llvm::ConstantInt::get(context_, llvm::APInt(32, 3));
        size = builder_.CreateShl(n, s);
      } else {
        std::uint64_t s = static_cast<std::uint16_t>(inst.d());
        s <<= 3;
        size = llvm::ConstantInt::get(context_, llvm::APInt(64, s));
      }
      auto array = builder_.CreateCall(malloc_fn, size);
      auto res =
          builder_.CreatePtrToInt(array, llvm::Type::getInt64Ty(context_));
      store_reg(inst.a(), res);
    } else if (o == Instruction::ADEL) {
      auto array = builder_.CreateIntToPtr(load_reg(inst.a()),
                                           llvm::Type::getInt64PtrTy(context_));
      builder_.CreateCall(free_fn, array);
    } else if (o >= Instruction::AGETR && o <= Instruction::ASETI) {
      auto array = builder_.CreateIntToPtr(load_reg(inst.b()),
                                           llvm::Type::getInt64PtrTy(context_));
      llvm::Value* index;
      if (o == Instruction::AGETR || o == Instruction::ASETR) {
        index = load_reg(inst.c());
      } else {
        ASSERT(o == Instruction::AGETI || o == Instruction::ASETI);
        auto val = inst.c();
        index = llvm::ConstantInt::get(context_, llvm::APInt(64, val, true));
      }
      auto gep = builder_.CreateGEP(array, index);
      if (o == Instruction::AGETR || o == Instruction::AGETI) {
        auto res = builder_.CreateLoad(gep);
        store_reg(inst.a(), res);
      } else {
        ASSERT(o == Instruction::ASETR || o == Instruction::ASETI);
        auto res = load_reg(inst.a());
        builder_.CreateStore(res, gep);
      }
    } else if (o == Instruction::JMP) {
      // The jump target (inst.d()) must be positive. Otherwise, it is a
      // backward jump to the loop header, which is prohibited in a trace.
      ASSERT_GT(inst.d(), 0);
    } else if (o == Instruction::JT || o == Instruction::JF) {
      // Generate guard.

      // Check the condition.
      auto val = load_reg(inst.a());
      auto zero = llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
      llvm::Value* cond;
      if (o == Instruction::JT) {
        cond = builder_.CreateICmpNE(val, zero);
      } else {
        cond = builder_.CreateICmpEQ(val, zero);
      }

      // Was the jump taken in the trace?
      ASSERT_LT(i + 1, trace.size());
      std::uint32_t next_pc = trace[i + 1];
      bool jump_taken = next_pc != pc + 1;

      // Generate corresponding branch.
      auto exit = llvm::BasicBlock::Create(context_, "exit", trace_fn);
      auto loop_cont = llvm::BasicBlock::Create(context_, "loop", trace_fn);
      if (jump_taken) {
        builder_.CreateCondBr(cond, loop_cont, exit);
      } else {
        builder_.CreateCondBr(cond, exit, loop_cont);
      }

      // Generate exit block.
      builder_.SetInsertPoint(exit);
      std::uint64_t target = pc + 1 + (jump_taken ? 0 : inst.d());
      std::uint64_t regs_offset = reg_offsets.back();
      std::uint64_t r = (regs_offset << 32) | target;
      auto ret_val = llvm::ConstantInt::get(context_, llvm::APInt(64, r));
      builder_.CreateStore(ret_val, ret);
      builder_.CreateBr(epilogue);

      // Continue emitting code in the loop.
      builder_.SetInsertPoint(loop_cont);

      // Update the trace's properties.
      if (!finish_ret) {
        finish_ret = r;
      } else {
        single_ret = false;
      }
    } else if (o == Instruction::CALL) {
      ASSERT_GE(inst.a(), 0);
      auto addr = reinterpret_cast<std::uint64_t>(&bytecode[pc]);
      // Store the return address, as the interpreter may need it in the case
      // if we exit the compiled trace in the function.
      auto val = llvm::ConstantInt::get(context_, llvm::APInt(64, addr, true));
      store_reg(inst.a(), val);
      // Shift registers, so that the registers used in the function cannot
      // overwrite the registers used in the current scope.
      auto regs_offset = reg_offsets.back() + inst.a() + 1;
      reg_offsets.push_back(regs_offset);
    } else if (o == Instruction::RETR || o == Instruction::RETI) {
      ASSERT_GE(reg_offsets.size(), 1);
      if (reg_offsets.size() == 1) {
        // A ret instruction in the loop. Exit the compiled code and handle it
        // in the interpreter.
        auto ret_val = llvm::ConstantInt::get(context_, llvm::APInt(64, pc));
        builder_.CreateStore(ret_val, ret);
        builder_.CreateBr(epilogue);
      } else {
        // A ret instruction in some function that was called (in)directly.
        // Save the result and restore the registers offset.
        llvm::Value* res;
        if (o == Instruction::RETR) {
          res = load_reg(inst.a());
        } else {
          auto val = static_cast<std::uint64_t>(inst.d());
          res = llvm::ConstantInt::get(context_, llvm::APInt(64, val, true));
        }
        store_reg(-1, res);
        reg_offsets.pop_back();
      }
    } else if (o == Instruction::LOOP) {
      ASSERT(0 && "LOOP instruction in the trace");
      UNREACHABLE();
    } else if (o == Instruction::LOOPC) {
      // A block for storing registers that were modified back to memory before
      // calling the nested trace.
      auto store = llvm::BasicBlock::Create(context_, "nested_store", trace_fn);
      builder_.CreateBr(store);

      // Call the nested trace.
      auto call = llvm::BasicBlock::Create(context_, "nested_call", trace_fn);
      builder_.SetInsertPoint(call);
      std::uint16_t index = inst.d();
      ASSERT_LT(index, constants.size());
      auto addr = constants[index];
      auto val = llvm::ConstantInt::get(context_, llvm::APInt(64, addr));
      auto type = llvm::PointerType::get(
          llvm::FunctionType::get(
              llvm::Type::getInt64Ty(context_),
              {llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(context_))},
              false),
          0);
      auto ptr = builder_.CreateIntToPtr(val, type);
      auto res = builder_.CreateCall(ptr, arg);

      // A block for loading registers after the call.
      auto load = llvm::BasicBlock::Create(context_, "nested_load", trace_fn);

      auto info = trace_info_[addr];
      if (info.single_ret) {
        // We can ignore the result.
        builder_.SetInsertPoint(call);
        builder_.CreateBr(load);
      } else {
        // We have to check whether the nested loop finished successfully or
        // some guard failed.
        auto finish_ret =
            llvm::ConstantInt::get(context_, llvm::APInt(64, info.finish_ret));
        auto cond = builder_.CreateICmpEQ(res, finish_ret);

        // An exit block, if a guard in the nested trace failed, which
        // propagates the returned value back to the interpreter.
        auto exit = llvm::BasicBlock::Create(context_, "exit", trace_fn);
        builder_.SetInsertPoint(exit);
        builder_.CreateStore(res, ret);
        builder_.CreateBr(epilogue);

        builder_.SetInsertPoint(call);
        builder_.CreateCondBr(cond, load, exit);

        single_ret = false;
      }

      auto loop_cont = llvm::BasicBlock::Create(context_, "loop", trace_fn);
      builder_.SetInsertPoint(loop_cont);

      // Add these block to generate loads/stores later.
      store_blocks.emplace_back(store, call);
      load_blocks.emplace_back(load, loop_cont);
    } else if (o == Instruction::EXIT || o == Instruction::OUT) {
      auto fn = o == Instruction::EXIT ? exit_fn : out_fn;
      auto val = load_reg(inst.a());
      builder_.CreateCall(fn, val);
    } else if (o == Instruction::IN) {
      auto val = builder_.CreateCall(in_fn);
      store_reg(inst.a(), val);
    }
  }
  // Go back to the loop's header.
  builder_.CreateBr(loop);

  // After emitting all required registers loads and allocas in the prologue
  // block, a branch to the loop can be created.
  builder_.SetInsertPoint(prologue);
  builder_.CreateBr(loop);

  // Fix the additional blocks for loading/storing registers.
  for (auto [load, dest] : load_blocks) {
    builder_.SetInsertPoint(load);
    for (auto reg : loaded_regs) {
      auto index = llvm::ConstantInt::get(context_, llvm::APInt(32, reg));
      auto ptr = builder_.CreateGEP(arg, index);
      auto value = builder_.CreateLoad(ptr);
      ASSERT_NE(regs.find(reg), regs.end());
      builder_.CreateStore(value, regs[reg]);
    }
    builder_.CreateBr(dest);
  }
  for (auto [store, dest] : store_blocks) {
    builder_.SetInsertPoint(store);
    for (auto reg : stored_regs) {
      ASSERT_NE(regs.find(reg), regs.end());
      auto value = builder_.CreateLoad(regs[reg]);
      auto index = llvm::ConstantInt::get(context_, llvm::APInt(32, reg));
      auto ptr = builder_.CreateGEP(arg, index);
      builder_.CreateStore(value, ptr);
    }
    builder_.CreateBr(dest);
  }

  // Create epilogue.
  trace_fn->getBasicBlockList().push_back(epilogue);
  builder_.SetInsertPoint(epilogue);
  // Update all modified registers.
  for (auto reg : stored_regs) {
    ASSERT_NE(regs.find(reg), regs.end());
    auto value = builder_.CreateLoad(regs[reg]);
    auto index = llvm::ConstantInt::get(context_, llvm::APInt(32, reg));
    auto ptr = builder_.CreateGEP(arg, index);
    builder_.CreateStore(value, ptr);
  }

  // Emit return.
  auto val = builder_.CreateLoad(ret);
  builder_.CreateRet(val);

  if (UNLIKELY(debug_flag != 0)) {
    std::fputs(
        "\n"
        "-------------------\n"
        "IR (not optimized):\n"
        "-------------------",
        stderr);
    trace_fn->print(llvm::errs());
  }

  optimize(module.get());

  if (UNLIKELY(debug_flag != 0)) {
    std::fputs(
        "\n"
        "---------------\n"
        "IR (optimized):\n"
        "---------------",
        stderr);
    trace_fn->print(llvm::errs());
  }

  // Compile.
  auto resolver = llvm::orc::createLambdaResolver(
      [&](const std::string& name) {
        if (auto sym = compile_layer_.findSymbol(name, false)) {
          return sym;
        }
        return llvm::JITSymbol(nullptr);
      },
      [](const std::string& name) {
        if (auto addr =
                llvm::RTDyldMemoryManager::getSymbolAddressInProcess(name))
          return llvm::JITSymbol(addr, llvm::JITSymbolFlags::Exported);
        return llvm::JITSymbol(nullptr);
      });
  auto handle = cantFail(
      compile_layer_.addModule(std::move(module), std::move(resolver)));
  auto addr = cantFail(handle->get()->getSymbol("trace", false).getAddress());

  ASSERT_EQ(finish_ret.has_value(), true);
  trace_info_[addr] = CodeGen::TraceInfo{*finish_ret, single_ret};

  return reinterpret_cast<std::uint64_t (*)(std::int64_t*)>(addr);
}

void CodeGen::optimize(llvm::Module* module) {
  llvm::legacy::FunctionPassManager fpm(module);
  fpm.add(llvm::createPromoteMemoryToRegisterPass());
  fpm.add(llvm::createIndVarSimplifyPass());
  fpm.add(llvm::createLICMPass());
  fpm.add(llvm::createLoopStrengthReducePass());
  fpm.add(llvm::createInstructionCombiningPass(true));
  fpm.add(llvm::createInstructionSimplifierPass());
  fpm.add(llvm::createReassociatePass());
  fpm.add(llvm::createNewGVNPass());
  fpm.add(llvm::createCFGSimplificationPass());
  fpm.doFinalization();

  for (auto& func : *module) {
    fpm.run(func);
  }
}
