#include "lexer.hpp"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string_view>

#include "cxx_extensions.hpp"

Lexer::Lexer(const char* source)
    : next_symbol_id_(NUM_TOKENS + 1), current_line_number_(1),
      current_(source + 1), last_(*source) {
  // Insert keywords.
  symbols_.emplace("array", ARRAY);
  symbols_.emplace("del", DEL);
  symbols_.emplace("else", ELSE);
  symbols_.emplace("fn", FN);
  symbols_.emplace("if", IF);
  symbols_.emplace("in", IN);
  symbols_.emplace("let", LET);
  symbols_.emplace("out", OUT);
  symbols_.emplace("return", RETURN);
  symbols_.emplace("while", WHILE);
}

void Lexer::consume_token() {
  int if_single;
  int if_double;
  for (;;) {
    if (std::isalpha(last_) || last_ == '_') {
      const char* lexeme = current_ - 1;
      do {
        last_ = *current_++;
      } while (std::isalnum(last_) || last_ == '_');
      const std::string_view symbol_name(lexeme, current_ - lexeme - 1);
      std::size_t symbol_id = find_or_insert_symbol(symbol_name);
      if (symbol_id < NUM_TOKENS) {
        token_ = symbol_id;
        return;
      }
      token_attribute_.sz = symbol_id;
      token_ = IDENTIFIER;
      return;
    }
    if (std::isdigit(last_)) {
      char* end;
      token_attribute_.i64 = std::strtoll(current_ - 1, &end, 0);
      current_ = end;
      last_ = *current_++;
      token_ = INTEGER_LITERAL;
      return;
    }
    switch (last_) {
    // Skip whitespaces.
    case '\n':
      ++current_line_number_;
      [[fallthrough]];
    case '\t':
    case '\v':
    case '\f':
    case '\r':
    case ' ':
      last_ = *current_++;
      continue;
    case '=':
      if_single = '=';
      if_double = EQ;
      goto relational_operators;
    case '!':
      if_single = '!';
      if_double = NE;
      goto relational_operators;
    case '<':
      if_single = '<';
      if_double = LE;
      goto relational_operators;
    case '>':
      if_single = '>';
      if_double = GE;
      goto relational_operators;
    default:
      token_ = last_;
      last_ = *current_++;
      return;
    }
  relational_operators:
    last_ = *current_++;
    if (last_ == '=') {
      last_ = *current_++;
      token_ = if_double;
      return;
    } else {
      token_ = if_single;
      return;
    }
  }
  UNREACHABLE();
}

void Lexer::check_and_consume_token(int token) {
  if (UNLIKELY(token_ != token)) {
    std::fprintf(stderr, "Error in line %zu: expected '",
                 current_line_number());
    print_token_name(token, stderr);
    std::fputs("', but got '", stderr);
    print_token_name(token_, stderr);
    std::fputs("'\n", stderr);
    std::exit(EXIT_FAILURE);
  }
  consume_token();
}

std::size_t Lexer::find_or_insert_symbol(std::string_view symbol_name) {
  auto it = symbols_.find(symbol_name);
  if (it != symbols_.end()) {
    return it->second;
  }
  symbols_.emplace(symbol_name, next_symbol_id_);
  return next_symbol_id_++;
}

std::size_t Lexer::current_line_number() const {
  return current_line_number_;
}

Lexer::TokenAttribute Lexer::token_attribute() const {
  return token_attribute_;
}

int Lexer::token() const {
  return token_;
}

void Lexer::print_symbol_name(std::size_t symbol_id, std::FILE* file) const {
  for (auto& [name, id] : symbols_) {
    if (id == symbol_id) {
      std::fwrite(name.data(), 1, name.size(), file);
      return;
    }
  }
  std::fputs("<unknown>", file);
}

void Lexer::print_token_name(int token, std::FILE* file) {
  switch (token) {
  case ARRAY:
    std::fputs("array", file);
    break;
  case DEL:
    std::fputs("del", file);
    break;
  case ELSE:
    std::fputs("else", file);
    break;
  case EQ:
    std::fputs("==", file);
    break;
  case FN:
    std::fputs("fn", file);
    break;
  case GE:
    std::fputs(">=", file);
    break;
  case IDENTIFIER:
    std::fputs("identifier", file);
    break;
  case IF:
    std::fputs("if", file);
    break;
  case IN:
    std::fputs("in", file);
    break;
  case INTEGER_LITERAL:
    std::fputs("integer_literal", file);
    break;
  case LE:
    std::fputs("<=", file);
    break;
  case LET:
    std::fputs("let", file);
    break;
  case NE:
    std::fputs("!=", file);
    break;
  case OUT:
    std::fputs("out", file);
    break;
  case RETURN:
    std::fputs("return", file);
    break;
  case WHILE:
    std::fputs("while", file);
    break;
  default:
    std::fputc(token, file);
    break;
  }
}
