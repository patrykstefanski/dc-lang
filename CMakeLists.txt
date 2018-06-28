cmake_minimum_required(VERSION 3.8)
project(dc-lang)

option(LINK_TIME_OPTIMIZATION "Link Time Optimization" OFF)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(LLVM_LIBS core native orcjit)
list(APPEND LIBS ${LLVM_LIBS})

add_subdirectory(lib/capstone)
include_directories(lib/capstone/include)
list(APPEND LIBS capstone-static)

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

  if(LINK_TIME_OPTIMIZATION)
    target_compile_options(${PROJECT_NAME} PRIVATE -flto)
    set_property(TARGET ${PROJECT_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -flto")
  endif()
endif()

if(UNIX)
  set_property(TARGET ${PROJECT_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -rdynamic")
endif()
