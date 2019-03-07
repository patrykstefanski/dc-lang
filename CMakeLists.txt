cmake_minimum_required(VERSION 3.8)
project(dc-lang)

option(DC_LANG_LTO "Enable LTO" OFF)
option(DC_LANG_USE_SYSTEM_LLVM "Use system LLVM libraries" ON)

add_subdirectory(lib/capstone)
include_directories(lib/capstone/include)
list(APPEND LIBS capstone-static)

if(DC_LANG_USE_SYSTEM_LLVM)
  find_package(LLVM REQUIRED CONFIG)
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})
else()
  set(LLVM_ABI_BREAKING_CHECKS "FORCE_OFF" CACHE STRING
      "Enable abi-breaking checks.  Can be WITH_ASSERTS, FORCE_ON or FORCE_OFF.")
  set(LLVM_BUILD_DOCS OFF CACHE BOOL "Build the llvm documentation.")
  set(LLVM_BUILD_EXAMPLES OFF CACHE BOOL "Build the LLVM example programs. If OFF, just generate build targets.")
  set(LLVM_BUILD_RUNTIME OFF CACHE BOOL "Build the LLVM runtime libraries.")
  set(LLVM_BUILD_RUNTIMES OFF CACHE BOOL "Build the LLVM runtimes. If OFF, just generate build targets.")
  set(LLVM_BUILD_TESTS OFF CACHE BOOL "Build LLVM unit tests. If OFF, just generate build targets.")
  set(LLVM_BUILD_TOOLS OFF CACHE BOOL "Build the LLVM tools. If OFF, just generate build targets.")
  set(LLVM_BUILD_UTILS OFF CACHE BOOL "Build LLVM utility binaries. If OFF, just generate build targets.")
  set(LLVM_ENABLE_BINDINGS OFF CACHE BOOL "Build bindings.")
  set(LLVM_ENABLE_CXX1Y ON CACHE BOOL "Compile with C++1y enabled.")
  set(LLVM_ENABLE_CXX1Z ON CACHE BOOL "Compile with C++1z enabled.")
  set(LLVM_ENABLE_LIBEDIT OFF CACHE BOOL "Use libedit if available.")
  set(LLVM_ENABLE_LIBPFM OFF CACHE BOOL "Use libpfm for performance counters if available.")
  set(LLVM_ENABLE_LIBXML2 "OFF" CACHE STRING "Use libxml2 if available. Can be ON, OFF, or FORCE_ON.")
  set(LLVM_ENABLE_ZLIB OFF CACHE BOOL "Use zlib for compression/decompression if available.")
  set(LLVM_INCLUDE_DOCS OFF CACHE BOOL "Generate build targets for the LLVM unit tests.")
  set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "Generate build targets for the LLVM examples.")
  set(LLVM_INCLUDE_GO_TESTS OFF CACHE BOOL "Include the Go bindings tests in test build targets.")
  set(LLVM_INCLUDE_RUNTIME OFF CACHE BOOL "Generate build targets for the LLVM runtimes.")
  set(LLVM_INCLUDE_RUNTIMES OFF CACHE BOOL "Generate build targets for the LLVM runtimes.")
  set(LLVM_INCLUDE_TESTS OFF CACHE BOOL "Generate build targets for the LLVM unit tests.")
  set(LLVM_INCLUDE_TOOLS OFF CACHE BOOL "Generate build targets for the LLVM tools.")
  set(LLVM_INCLUDE_UTILS OFF CACHE BOOL "Generate build targets for the LLVM utils.")
  set(LLVM_TARGETS_TO_BUILD "Native" CACHE STRING "Semicolon-separated list of targets to build, or \"all\".")
  add_subdirectory("lib/llvm-project/llvm")
  include_directories("lib/llvm-project/llvm/include")
endif()
llvm_map_components_to_libnames(LLVM_LIBS
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  ScalarOpts
  Support
  X86AsmParser
  X86CodeGen)
list(APPEND LIBS ${LLVM_LIBS})

set(SOURCES
  src/code_gen.cpp
  src/instruction.cpp
  src/interpreter.cpp
  src/lexer.cpp
  src/main.cpp
  src/parser.cpp
  src/utilities.cpp)

add_executable(${PROJECT_NAME} ${SOURCES})
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 17)
target_link_libraries(${PROJECT_NAME} ${LIBS})

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_COMPILER_IS_GNUCXX)
  target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -fno-exceptions -fno-rtti -fno-stack-protector -fomit-frame-pointer)

  if(DC_LANG_LTO)
    target_compile_options(${PROJECT_NAME} PRIVATE -flto)
    set_property(TARGET ${PROJECT_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -flto")
  endif()
endif()

if(UNIX)
  set_property(TARGET ${PROJECT_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -rdynamic")
endif()
