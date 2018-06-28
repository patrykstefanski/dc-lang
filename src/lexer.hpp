#ifndef LEXER_HPP
#define LEXER_HPP

#include <cstdint>
#include <cstdio>
#include <string_view>
#include <unordered_map>

#include "cxx_extensions.hpp"

class Lexer {
public:
  enum Token {
    ARRAY = 256,
    DEL,
    ELSE,
    EQ,
    FN,
    GE,
    IDENTIFIER,
    IF,
    IN,
    INTEGER_LITERAL,
    LE,
    LET,
    NE,
    OUT,
    RETURN,
    WHILE,
    NUM_TOKENS,
  };

  union TokenAttribute {
    std::size_t sz;
    std::int64_t i64;
  };

  explicit Lexer(const char* source);
  Lexer(const Lexer&) = default;
  Lexer& operator=(const Lexer&) = default;
  Lexer(Lexer&&) = default;
  Lexer& operator=(Lexer&&) = default;

  // Consumes the next token.
  void consume_token();

  // Checks the current token and consumes the next one.
  void check_and_consume_token(int token);

  // Finds or inserts the symbol, returns the symbol id.
  std::size_t find_or_insert_symbol(std::string_view symbol_name);

  std::size_t current_line_number() const;
  TokenAttribute token_attribute() const;
  int token() const;

  // Prints the symbol name for the given symbol id. This function is used
  // only to print errors.
  COLD void print_symbol_name(std::size_t symbol_id, std::FILE* file) const;

  // Prints the token name. This function is used only to print errors.
  COLD static void print_token_name(int token, std::FILE* file);

private:
  void next_char();

  std::unordered_map<std::string_view, std::size_t> symbols_;
  std::size_t next_symbol_id_;
  std::size_t current_line_number_;
  const char* current_;
  TokenAttribute token_attribute_;
  int token_;
  char last_;
};

#endif  // !LEXER_HPP
