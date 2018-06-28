#include "parser.hpp"

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <string_view>
#include <utility>
#include <vector>

#include "assert.hpp"
#include "instruction.hpp"
#include "lexer.hpp"

Parser::Parser(Lexer lexer) : lexer_(std::move(lexer)) {
}

const std::vector<Instruction>& Parser::bytecode() const {
  return bytecode_;
}

const std::vector<std::int64_t>& Parser::constants() const {
  return constants_;
}

void Parser::parse() {
  // Emit the program prolog that calls main and exits.
  std::string_view main{"main", sizeof("main") - 1};
  std::size_t symbol_id = lexer_.find_or_insert_symbol(main);
  std::uint8_t reg = 0;
  std::size_t num_params = 0;
  emit_call(symbol_id, reg, num_params);
  bytecode_.push_back(Instruction::make_ad(Instruction::EXIT, reg, 0));
  // Perform passes.
  first_pass();
  second_pass();
}

void Parser::first_pass() {
  lexer_.consume_token();
  while (lexer_.token() != 0) {
    parse_fn();
  }
}

void Parser::second_pass() {
  // Patch function calls.
  for (std::size_t i = 0; i < bytecode_.size(); ++i) {
    auto call_instruction = bytecode_[i];
    if (LIKELY(call_instruction.opcode() != Instruction::CALL)) {
      continue;
    }
    // There must be const instruction with symbol id.
    ASSERT_GT(i, 0);
    auto const_instruction = bytecode_[i - 1];
    ASSERT_EQ(const_instruction.opcode(), Instruction::MOVI);
    // Find the function.
    std::size_t symbol_id = const_instruction.d();
    auto it = functions_.find(symbol_id);
    if (UNLIKELY(it == functions_.end())) {
      std::fputs("Error: function '", stderr);
      lexer_.print_symbol_name(symbol_id, stderr);
      std::fputs("' undefined\n", stderr);
      std::exit(EXIT_FAILURE);
    }
    // Check the number of arguments.
    std::size_t num_required_args = it->second.second;
    std::size_t num_given_args = call_instruction.b();
    if (UNLIKELY(num_required_args != num_given_args)) {
      std::fputs("Error: function '", stderr);
      lexer_.print_symbol_name(symbol_id, stderr);
      std::fprintf(stderr, "' requires %zu arguments, but %zu given\n",
                   num_required_args, num_given_args);
      std::exit(EXIT_FAILURE);
    }
    // Patch the const instruction for call.
    std::size_t pos = it->second.first;
    std::size_t offset = pos - i - 1;
    bytecode_[i - 1].set_d(offset);
  }
}

void Parser::parse_fn() {
  lexer_.check_and_consume_token(Lexer::FN);
  // Begin a new scope.
  Parser::Scope new_scope;
  current_scope_ = &new_scope;
  // Parse function name.
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.check_and_consume_token(Lexer::IDENTIFIER);
  // Parse arguments.
  lexer_.check_and_consume_token('(');
  std::size_t num_args = parse_arguments();
  ASSERT_EQ(lexer_.token(), ')');
  lexer_.consume_token();
  // Register the function.
  auto pos_num_args_pair = std::make_pair(bytecode_.size(), num_args);
  bool inserted = functions_.emplace(symbol_id, pos_num_args_pair).second;
  if (UNLIKELY(!inserted)) {
    std::fprintf(stderr, "Error in line %zu: function '",
                 lexer_.current_line_number());
    lexer_.print_symbol_name(symbol_id, stderr);
    std::fputs("' redefined\n", stderr);
    std::exit(EXIT_FAILURE);
  }
  // Parse the body.
  parse_block();
  // Generate return for void functions and for jump list.
  auto expr = Parser::Expression::make_value(0);
  emit_return(expr);
  // End the scope.
  current_scope_ = nullptr;
}

std::size_t Parser::parse_arguments() {
  if (lexer_.token() == ')') {
    // No arguments.
    return 0;
  }
  std::size_t num_args = 0;
  for (;;) {
    // Parse a parameter name.
    std::size_t symbol_id = lexer_.token_attribute().sz;
    lexer_.check_and_consume_token(Lexer::IDENTIFIER);
    // Add the variable.
    variable_regs_[current_scope_->num_variables_++] = symbol_id;
    ++num_args;
    // Check whether there are another parameters.
    if (lexer_.token() == ')') {
      break;
    }
    if (UNLIKELY(lexer_.token() != ',')) {
      std::fprintf(stderr, "Error in line %zu: expected ')' or ',' but got'",
                   lexer_.current_line_number());
      lexer_.print_token_name(lexer_.token(), stderr);
      std::fputs("'\n", stderr);
      std::exit(EXIT_FAILURE);
    }
    lexer_.consume_token();
  }
  // Store the arguments in next registers.
  current_scope_->first_free_reg_ += num_args;
  ASSERT_EQ(current_scope_->first_free_reg_, current_scope_->num_variables_);
  return num_args;
}

bool Parser::parse_statement() {
  switch (lexer_.token()) {
  case '{':
    parse_block();
    return true;
  case Lexer::IF:
    parse_if();
    return true;
  case Lexer::WHILE:
    parse_while();
    return true;
  case Lexer::LET:
    parse_let();
    return true;
  case Lexer::ARRAY:
    parse_array();
    return true;
  case Lexer::DEL:
    parse_del();
    return true;
  case Lexer::RETURN:
    parse_return();
    return false;
  case Lexer::IN:
    parse_in();
    return true;
  case Lexer::OUT:
    parse_out();
    return true;
  default:
    parse_assignment_or_call();
    return true;
  }
}

void Parser::parse_block() {
  lexer_.check_and_consume_token('{');
  // Begin a new scope and parse a chunk.
  auto* old_scope = current_scope_;
  Parser::Scope new_scope(*old_scope);
  current_scope_ = &new_scope;
  // Parse statements.
  while (lexer_.token() != '}' && parse_statement())
    ;
  lexer_.check_and_consume_token('}');
  // End the scope.
  current_scope_ = old_scope;
}

void Parser::parse_if() {
  ASSERT_EQ(lexer_.token(), Lexer::IF);
  lexer_.consume_token();
  std::size_t escape_list = static_cast<std::size_t>(-1);
  std::size_t cond_jump = parse_cond_block();
  // Parse 'else if'.
  bool was_else;
  while ((was_else = lexer_.token() == Lexer::ELSE) &&
         (lexer_.consume_token(), lexer_.token() == Lexer::IF)) {
    append_jump(&escape_list, emit_unconditional_jump());
    patch_jump_list_to_here(cond_jump);
    lexer_.consume_token();
    cond_jump = parse_cond_block();
  }
  if (was_else) {
    append_jump(&escape_list, emit_unconditional_jump());
    patch_jump_list_to_here(cond_jump);
    parse_block();
  } else {
    append_jump(&escape_list, cond_jump);
  }
  patch_jump_list_to_here(escape_list);
}

std::size_t Parser::parse_cond_block() {
  auto expr = parse_expr();
  expr = expr_to_any_reg(expr);
  free_expr_reg(expr);
  std::size_t pos = emit_conditional_jump(expr.reg(), false);
  parse_block();
  return pos;
}

void Parser::parse_while() {
  ASSERT_EQ(lexer_.token(), Lexer::WHILE);
  lexer_.consume_token();
  // Save the program counter of the beginning of the conditional expression.
  std::size_t start = bytecode_.size();
  // Emit loop instruction.
  bytecode_.push_back(Instruction::make_ad(Instruction::LOOP, 0, 0));
  // Parse expression and emit skip jump.
  auto expr = parse_expr();
  expr = expr_to_any_reg(expr);
  free_expr_reg(expr);
  std::size_t exit = emit_conditional_jump(expr.reg(), false);
  // Parse the loop block.
  parse_block();
  // Jump to the start of the loop.
  patch_single_jump(emit_unconditional_jump(), start);
  // Patch the exit jump.
  patch_jump_list_to_here(exit);
}

void Parser::parse_let() {
  ASSERT_EQ(lexer_.token(), Lexer::LET);
  lexer_.consume_token();
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.check_and_consume_token(Lexer::IDENTIFIER);
  lexer_.check_and_consume_token('=');
  auto expr = parse_expr();
  free_expr_reg(expr);
  expr = expr_to_next_reg(expr);
  // Add variable.
  variable_regs_[current_scope_->num_variables_++] = symbol_id;
  ASSERT_EQ(current_scope_->first_free_reg_, current_scope_->num_variables_);
  lexer_.check_and_consume_token(';');
}

void Parser::parse_array() {
  ASSERT_EQ(lexer_.token(), Lexer::ARRAY);
  lexer_.consume_token();
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.check_and_consume_token(Lexer::IDENTIFIER);
  lexer_.check_and_consume_token('[');
  auto expr = parse_expr();
  lexer_.check_and_consume_token(']');
  // Emit.
  emit_array_new(expr);
  // Add variable.
  variable_regs_[current_scope_->num_variables_++] = symbol_id;
  ASSERT_EQ(current_scope_->first_free_reg_, current_scope_->num_variables_);
  lexer_.check_and_consume_token(';');
}

void Parser::parse_del() {
  ASSERT_EQ(lexer_.token(), Lexer::DEL);
  lexer_.consume_token();
  auto expr = parse_expr();
  emit_array_delete(expr);
  lexer_.check_and_consume_token(';');
}

void Parser::parse_return() {
  ASSERT_EQ(lexer_.token(), Lexer::RETURN);
  lexer_.consume_token();
  auto expr = parse_expr();
  emit_return(expr);
  lexer_.check_and_consume_token(';');
}

void Parser::parse_in() {
  ASSERT_EQ(lexer_.token(), Lexer::IN);
  lexer_.consume_token();
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.check_and_consume_token(Lexer::IDENTIFIER);
  std::size_t reg = find_variable_reg(symbol_id);
  if (UNLIKELY(reg == std::numeric_limits<std::size_t>::max())) {
    std::fprintf(stderr, "Error in line %zu: the symbol '",
                 lexer_.current_line_number());
    lexer_.print_symbol_name(symbol_id, stderr);
    std::fputs("' doesn't exist in the current scope\n", stderr);
    std::exit(EXIT_FAILURE);
  }
  emit_io(Instruction::IN, reg);
  lexer_.check_and_consume_token(';');
}

void Parser::parse_out() {
  ASSERT_EQ(lexer_.token(), Lexer::OUT);
  lexer_.consume_token();
  auto expr = parse_expr();
  expr = expr_to_any_reg(expr);
  free_expr_reg(expr);
  emit_io(Instruction::OUT, expr.reg());
  lexer_.check_and_consume_token(';');
}

void Parser::parse_assignment_or_call() {
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.check_and_consume_token(Lexer::IDENTIFIER);
  if (lexer_.token() == '(') {
    // A function call.
    std::uint8_t reg = current_scope_->first_free_reg_++;
    lexer_.consume_token();
    std::size_t num_params = parse_parameters();
    ASSERT_EQ(lexer_.token(), ')');
    lexer_.consume_token();
    emit_call(symbol_id, reg, num_params);
    --current_scope_->first_free_reg_;
  } else {
    // A simple variable or an array[index] assignment.
    std::size_t reg = find_variable_reg(symbol_id);
    if (UNLIKELY(reg == std::numeric_limits<std::size_t>::max())) {
      std::fprintf(stderr, "Error in line %zu: the symbol '",
                   lexer_.current_line_number());
      lexer_.print_symbol_name(symbol_id, stderr);
      std::fputs("' doesn't exist in the current scope\n", stderr);
      std::exit(EXIT_FAILURE);
    }
    if (lexer_.token() == '[') {
      // An array assignment.
      lexer_.consume_token();
      auto index_expr = parse_expr();
      lexer_.check_and_consume_token(']');
      lexer_.check_and_consume_token('=');
      auto expr = parse_expr();
      emit_array_set(reg, index_expr, expr);
    } else {
      // A simple variable assignment.
      lexer_.check_and_consume_token('=');
      auto expr = parse_expr();
      free_expr_reg(expr);
      expr_to_reg(expr, reg);
    }
  }
  ASSERT_EQ(current_scope_->first_free_reg_, current_scope_->num_variables_);
  lexer_.check_and_consume_token(';');
}

std::size_t Parser::parse_parameters() {
  if (lexer_.token() == ')') {
    // No parameters.
    return 0;
  }
  std::size_t num_params = 0;
  for (;;) {
    auto expr = parse_expr();
    free_expr_reg(expr);
    expr_to_next_reg(expr);
    ++num_params;
    if (lexer_.token() == ')') {
      break;
    }
    if (UNLIKELY(lexer_.token() != ',')) {
      std::fprintf(stderr, "Error in line %zu: expected ')' or ',' but got'",
                   lexer_.current_line_number());
      lexer_.print_token_name(lexer_.token(), stderr);
      std::fputs("'\n", stderr);
      std::exit(EXIT_FAILURE);
    }
    lexer_.consume_token();
  }
  // Free allocated registers for parameters.
  current_scope_->first_free_reg_ -= num_params;
  return num_params;
}

Parser::Expression Parser::parse_expr() {
  // Set limit to 0, i.e. accept all operators.
  return parse_binary_expr(0);
}

Parser::Expression Parser::parse_binary_expr(std::size_t limit) {
  static const std::size_t operator_precedence[] = {
      /* ADD */ 2,
      /* MUL */ 3,
      /* EQ */ 1,
      /* NE */ 1,
      /* SUB */ 2,
      /* DIV */ 3,
      /* MOD */ 3,
      /* LT */ 1,
      /* LE */ 1,
      /* GT */ 1,
      /* GE */ 1,
  };
  auto lhs = parse_unary_expr();
  int op = token_to_binary_op(lexer_.token());
  while (op != -1 && operator_precedence[op] > limit) {
    // Consume the operator.
    lexer_.consume_token();
    // Parse a binary expression with higher precedence.
    auto rhs = parse_binary_expr(operator_precedence[op]);
    // Emit the operator.
    lhs = emit_binary_op(op, lhs, rhs);
    // Continue parsing the expression.
    op = token_to_binary_op(lexer_.token());
  }
  return lhs;
}

Parser::Expression Parser::parse_unary_expr() {
  int op;
  switch (lexer_.token()) {
  case '!':
    op = Parser::NOT;
    break;
  case '-':
    op = Parser::NEG;
    break;
  default:
    return parse_primary_expr();
  }
  lexer_.consume_token();
  auto expr = parse_primary_expr();
  expr = emit_unary_op(op, expr);
  return expr;
}

Parser::Expression Parser::parse_primary_expr() {
  switch (lexer_.token()) {
  case '(':
    return parse_parentheses_expr();
  case Lexer::IDENTIFIER:
    return parse_identifier_expr();
  case Lexer::INTEGER_LITERAL:
    return parse_integer_literal_expr();
  default:
    std::fprintf(stderr, "Error in line %zu: expected '",
                 lexer_.current_line_number());
    lexer_.print_token_name(Lexer::IDENTIFIER, stderr);
    std::fputs("', '", stderr);
    lexer_.print_token_name(Lexer::INTEGER_LITERAL, stderr);
    std::fputs("' or '(' but got '", stderr);
    lexer_.print_token_name(lexer_.token(), stderr);
    std::fputs("'\n", stderr);
    std::exit(EXIT_FAILURE);
  }
}

Parser::Expression Parser::parse_parentheses_expr() {
  ASSERT_EQ(lexer_.token(), '(');
  lexer_.consume_token();
  auto expr = parse_expr();
  lexer_.check_and_consume_token(')');
  return expr;
}

Parser::Expression Parser::parse_identifier_expr() {
  ASSERT_EQ(lexer_.token(), Lexer::IDENTIFIER);
  std::size_t symbol_id = lexer_.token_attribute().sz;
  lexer_.consume_token();
  if (lexer_.token() == '(') {
    // A function.
    std::uint8_t reg = current_scope_->first_free_reg_++;
    lexer_.consume_token();
    std::size_t num_params = parse_parameters();
    ASSERT_EQ(lexer_.token(), ')');
    lexer_.consume_token();
    return emit_call(symbol_id, reg, num_params);
  }
  // A simple variable or an array[index].
  std::size_t reg = find_variable_reg(symbol_id);
  if (UNLIKELY(reg == std::numeric_limits<std::size_t>::max())) {
    std::fprintf(stderr, "Error in line %zu: the symbol '",
                 lexer_.current_line_number());
    lexer_.print_symbol_name(symbol_id, stderr);
    std::fputs("' doesn't exist in the current scope\n", stderr);
    std::exit(EXIT_FAILURE);
  }
  if (lexer_.token() == '[') {
    // An array get.
    lexer_.consume_token();
    auto index_expr = parse_expr();
    lexer_.check_and_consume_token(']');
    return emit_array_get(reg, index_expr);
  }
  // A simple variable.
  return Parser::Expression::make_reg(reg);
}

Parser::Expression Parser::parse_integer_literal_expr() {
  ASSERT_EQ(lexer_.token(), Lexer::INTEGER_LITERAL);
  std::int64_t value = lexer_.token_attribute().i64;
  lexer_.consume_token();
  return Parser::Expression::make_value(value);
}

// Variables and registers.

std::size_t Parser::find_variable_reg(std::size_t symbol_id) const {
  std::size_t i = current_scope_->num_variables_;
  while (i-- != 0) {
    if (variable_regs_[i] == symbol_id) {
      return i;
    }
  }
  return std::numeric_limits<std::size_t>::max();
}

Parser::Expression Parser::expr_to_reg(Parser::Expression expr,
                                       std::uint8_t reg) {
  if (!expr.has_reg()) {
    if (UNLIKELY(expr.value() < std::numeric_limits<std::int16_t>::min() ||
                 expr.value() > std::numeric_limits<std::int16_t>::max())) {
      ASSERT_LE(constants_.size(), std::numeric_limits<std::uint16_t>::max());
      std::uint16_t index = constants_.size();
      constants_.push_back(expr.value());
      bytecode_.push_back(Instruction::make_ad(Instruction::CONST, reg, index));
    } else {
      bytecode_.push_back(
          Instruction::make_ad(Instruction::MOVI, reg, expr.value()));
    }
  } else if (expr.reg() != reg) {
    bytecode_.push_back(
        Instruction::make_abc(Instruction::MOVR, reg, expr.reg(), 0));
  }
  return Parser::Expression::make_reg(reg);
}

Parser::Expression Parser::expr_to_next_reg(Parser::Expression expr) {
  std::uint8_t reg = current_scope_->first_free_reg_++;
  return expr_to_reg(expr, reg);
}

Parser::Expression Parser::expr_to_any_reg(Parser::Expression expr) {
  if (expr.has_reg()) {
    return expr;
  }
  return expr_to_next_reg(expr);
}

void Parser::free_expr_reg(Parser::Expression expr) {
  if (expr.has_reg() && expr.reg() >= current_scope_->num_variables_) {
    --current_scope_->first_free_reg_;
  }
}

// Jump lists.

std::size_t Parser::next_jump(std::size_t pos) const {
  const auto& instruction = bytecode_[pos];
  std::int16_t delta = instruction.d();
  if (delta == -1) {
    return -1;
  }
  return pos + 1 + delta;
}

void Parser::append_jump(std::size_t* list, std::size_t pos) {
  if (*list == static_cast<std::size_t>(-1)) {
    *list = pos;
    return;
  }
  std::size_t cur = *list;
  std::size_t next;
  while ((next = next_jump(cur)) != static_cast<std::size_t>(-1)) {
    cur = next;
  }
  patch_single_jump(cur, pos);
}

void Parser::patch_single_jump(std::size_t pos, std::size_t target) {
  std::int16_t offset = target - (pos + 1);
  bytecode_[pos].set_d(offset);
}

void Parser::patch_jump_list(std::size_t list, std::size_t target) {
  while (list != static_cast<std::size_t>(-1)) {
    std::size_t next = next_jump(list);
    patch_single_jump(list, target);
    list = next;
  }
}

void Parser::patch_jump_list_to_here(std::size_t list) {
  patch_jump_list(list, bytecode_.size());
}

// Bytecode emitting.

Parser::Expression Parser::emit_binary_op(int op, Parser::Expression lhs,
                                          Parser::Expression rhs) {
  ASSERT_GE(op, Parser::ADD);
  ASSERT_LE(op, Parser::GE);
  if (!lhs.has_reg() && !rhs.has_reg()) {
    return fold_binary_op(op, lhs, rhs);
  }
  // Change '>', '>=' to '<', '<=', as this is allowed for integers.
  if (op >= Parser::GT) {
    std::swap(lhs, rhs);
    op = op - Parser::GT + Parser::LT;
  }
  if (op < Parser::SUB) {
    return emit_commutative_op(op, lhs, rhs);
  } else {
    return emit_noncommutative_op(op, lhs, rhs);
  }
}

Parser::Expression Parser::emit_commutative_op(int op, Parser::Expression lhs,
                                               Parser::Expression rhs) {
  ASSERT_GE(op, Parser::ADD);
  ASSERT_LE(op, Parser::NE);
  bool use_imm_instruction = false;
  if (!rhs.has_reg() &&
      rhs.value() >= std::numeric_limits<std::int8_t>::min() &&
      rhs.value() <= std::numeric_limits<std::int8_t>::max()) {
    use_imm_instruction = true;
  } else if (!lhs.has_reg() &&
             lhs.value() >= std::numeric_limits<std::int8_t>::min() &&
             lhs.value() <= std::numeric_limits<std::int8_t>::max()) {
    std::swap(lhs, rhs);
    use_imm_instruction = true;
  }
  Instruction::Opcode opcode;
  std::uint8_t c;
  if (use_imm_instruction) {
    lhs = expr_to_any_reg(lhs);
    free_expr_reg(lhs);
    opcode =
        static_cast<Instruction::Opcode>(op - Parser::ADD + Instruction::ADDRI);
    c = rhs.value();
  } else {
    rhs = expr_to_any_reg(rhs);
    lhs = expr_to_any_reg(lhs);
    free_expr_reg(lhs);
    free_expr_reg(rhs);
    opcode =
        static_cast<Instruction::Opcode>(op - Parser::ADD + Instruction::ADDRR);
    c = rhs.reg();
  }
  std::uint8_t reg = current_scope_->first_free_reg_++;
  bytecode_.push_back(Instruction::make_abc(opcode, reg, lhs.reg(), c));
  return Expression::make_reg(reg);
}

Parser::Expression Parser::emit_noncommutative_op(int op,
                                                  Parser::Expression lhs,
                                                  Parser::Expression rhs) {
  ASSERT_GE(op, Parser::SUB);
  ASSERT_LE(op, Parser::LE);
  Instruction::Opcode opcode;
  std::uint8_t b;
  std::uint8_t c;
  if (!rhs.has_reg() &&
      rhs.value() >= std::numeric_limits<std::int8_t>::min() &&
      rhs.value() <= std::numeric_limits<std::int8_t>::max()) {
    lhs = expr_to_any_reg(lhs);
    free_expr_reg(lhs);
    opcode =
        static_cast<Instruction::Opcode>(op - Parser::SUB + Instruction::SUBRI);
    b = lhs.reg();
    c = rhs.value();
  } else if (!lhs.has_reg() &&
             lhs.value() >= std::numeric_limits<std::int8_t>::min() &&
             lhs.value() <= std::numeric_limits<std::int8_t>::max()) {
    rhs = expr_to_any_reg(rhs);
    free_expr_reg(rhs);
    opcode =
        static_cast<Instruction::Opcode>(op - Parser::SUB + Instruction::SUBIR);
    b = lhs.value();
    c = rhs.reg();
  } else {
    rhs = expr_to_any_reg(rhs);
    lhs = expr_to_any_reg(lhs);
    free_expr_reg(lhs);
    free_expr_reg(rhs);
    opcode =
        static_cast<Instruction::Opcode>(op - Parser::SUB + Instruction::SUBRR);
    b = lhs.reg();
    c = rhs.reg();
  }
  std::uint8_t reg = current_scope_->first_free_reg_++;
  bytecode_.push_back(Instruction::make_abc(opcode, reg, b, c));
  return Expression::make_reg(reg);
}

Parser::Expression Parser::emit_unary_op(int op, Parser::Expression expr) {
  ASSERT_GE(op, Parser::NEG);
  ASSERT_LE(op, Parser::NOT);
  if (!expr.has_reg()) {
    return fold_unary_op(op, expr);
  }
  expr = expr_to_any_reg(expr);
  free_expr_reg(expr);
  std::uint8_t reg = current_scope_->first_free_reg_++;
  auto opcode =
      static_cast<Instruction::Opcode>(op - Parser::NEG + Instruction::NEG);
  bytecode_.push_back(Instruction::make_abc(opcode, reg, expr.reg(), 0));
  return Parser::Expression::make_reg(reg);
}

std::size_t Parser::emit_unconditional_jump() {
  std::size_t pos = bytecode_.size();
  bytecode_.push_back(Instruction::make_ad(Instruction::JMP, 0, -1));
  return pos;
}

std::size_t Parser::emit_conditional_jump(std::uint8_t reg, bool condition) {
  std::size_t pos = bytecode_.size();
  auto opcode = condition ? Instruction::JT : Instruction::JF;
  bytecode_.push_back(Instruction::make_ad(opcode, reg, -1));
  return pos;
}

Parser::Expression Parser::emit_call(std::size_t symbol_id, std::uint8_t reg,
                                     std::size_t num_args) {
  if (UNLIKELY(num_args > std::numeric_limits<std::uint8_t>::max())) {
    std::fputs("Error: too many arguments passed to the '", stderr);
    lexer_.print_symbol_name(symbol_id, stderr);
    std::fputs("' function\n", stderr);
    std::exit(EXIT_FAILURE);
  }
  auto expr = Parser::Expression::make_value(symbol_id);
  expr = expr_to_reg(expr, reg);
  bytecode_.push_back(
      Instruction::make_abc(Instruction::CALL, reg, num_args, 0));
  return expr;
}

void Parser::emit_return(Parser::Expression expr) {
  if (!expr.has_reg() &&
      expr.value() >= std::numeric_limits<std::int16_t>::min() &&
      expr.value() <= std::numeric_limits<std::int16_t>::max()) {
    bytecode_.push_back(
        Instruction::make_ad(Instruction::RETI, 0, expr.value()));
  } else {
    expr = expr_to_any_reg(expr);
    free_expr_reg(expr);
    bytecode_.push_back(
        Instruction::make_abc(Instruction::RETR, expr.reg(), 0, 0));
  }
}

void Parser::emit_array_new(Parser::Expression size_expr) {
  if (!size_expr.has_reg() && size_expr.value() >= 0 &&
      size_expr.value() <= std::numeric_limits<std::uint16_t>::max()) {
    std::uint8_t array_reg = current_scope_->first_free_reg_++;
    std::uint16_t size = size_expr.value();
    bytecode_.push_back(
        Instruction::make_ad(Instruction::ANEWI, array_reg, size));
  } else {
    size_expr = expr_to_any_reg(size_expr);
    free_expr_reg(size_expr);
    std::uint8_t size_reg = size_expr.reg();
    std::uint8_t array_reg = current_scope_->first_free_reg_++;
    bytecode_.push_back(
        Instruction::make_abc(Instruction::ANEWR, array_reg, size_reg, 0));
  }
}

void Parser::emit_array_delete(Parser::Expression expr) {
  expr = expr_to_any_reg(expr);
  free_expr_reg(expr);
  bytecode_.push_back(Instruction::make_ad(Instruction::ADEL, expr.reg(), 0));
}

Parser::Expression Parser::emit_array_get(std::uint8_t array_reg,
                                          Parser::Expression index_expr) {
  Instruction::Opcode opcode;
  std::uint8_t c;
  if (!index_expr.has_reg() &&
      index_expr.value() >= std::numeric_limits<std::int8_t>::min() &&
      index_expr.value() <= std::numeric_limits<std::int8_t>::max()) {
    opcode = Instruction::AGETI;
    c = index_expr.value();
  } else {
    opcode = Instruction::AGETR;
    index_expr = expr_to_any_reg(index_expr);
    c = index_expr.reg();
    free_expr_reg(index_expr);
  }
  std::uint8_t reg = current_scope_->first_free_reg_++;
  bytecode_.push_back(Instruction::make_abc(opcode, reg, array_reg, c));
  return Expression::make_reg(reg);
}

void Parser::emit_array_set(std::uint8_t array_reg,
                            Parser::Expression index_expr,
                            Parser::Expression value_expr) {
  Instruction::Opcode opcode;
  std::uint8_t c;
  if (!index_expr.has_reg() &&
      index_expr.value() >= std::numeric_limits<std::int8_t>::min() &&
      index_expr.value() <= std::numeric_limits<std::int8_t>::max()) {
    opcode = Instruction::ASETI;
    c = index_expr.value();
  } else {
    opcode = Instruction::ASETR;
    index_expr = expr_to_any_reg(index_expr);
    c = index_expr.reg();
  }
  value_expr = expr_to_any_reg(value_expr);
  std::uint8_t a = value_expr.reg();
  free_expr_reg(value_expr);
  free_expr_reg(index_expr);
  bytecode_.push_back(Instruction::make_abc(opcode, a, array_reg, c));
}

void Parser::emit_io(Instruction::Opcode opcode, std::uint8_t reg) {
  ASSERT(opcode == Instruction::IN || opcode == Instruction::OUT);
  bytecode_.push_back(Instruction::make_abc(opcode, reg, 0, 0));
}

// Utilities.

int Parser::token_to_binary_op(int token) {
  switch (token) {
  case '%':
    return Parser::MOD;
  case '*':
    return Parser::MUL;
  case '+':
    return Parser::ADD;
  case '-':
    return Parser::SUB;
  case '/':
    return Parser::DIV;
  case '<':
    return Parser::LT;
  case '>':
    return Parser::GT;
  case Lexer::EQ:
    return Parser::EQ;
  case Lexer::NE:
    return Parser::NE;
  case Lexer::LE:
    return Parser::LE;
  case Lexer::GE:
    return Parser::GE;
  default:
    return -1;
  }
  UNREACHABLE();
}

Parser::Expression Parser::fold_binary_op(int op, Parser::Expression lhs,
                                          Parser::Expression rhs) {
  ASSERT_GE(op, Parser::ADD);
  ASSERT_LE(op, Parser::GE);
  ASSERT_EQ(lhs.has_reg(), false);
  ASSERT_EQ(rhs.has_reg(), false);
  std::int64_t value;
  switch (op) {
  case Parser::ADD:
    value = lhs.value() + rhs.value();
    break;
  case Parser::SUB:
    value = lhs.value() - rhs.value();
    break;
  case Parser::MUL:
    value = lhs.value() * rhs.value();
    break;
  case Parser::DIV:
    value = lhs.value() / rhs.value();
    break;
  case Parser::MOD:
    value = lhs.value() % rhs.value();
    break;
  case Parser::EQ:
    value = lhs.value() == rhs.value();
    break;
  case Parser::NE:
    value = lhs.value() != rhs.value();
    break;
  case Parser::LT:
    value = lhs.value() < rhs.value();
    break;
  case Parser::LE:
    value = lhs.value() <= rhs.value();
    break;
  case Parser::GT:
    value = lhs.value() > rhs.value();
    break;
  case Parser::GE:
    value = lhs.value() >= rhs.value();
    break;
  default:
    UNREACHABLE();
  }
  return Parser::Expression::make_value(value);
}

Parser::Expression Parser::fold_unary_op(int op, Parser::Expression expr) {
  ASSERT_GE(op, Parser::NEG);
  ASSERT_LE(op, Parser::NOT);
  ASSERT_EQ(expr.has_reg(), false);
  std::int64_t value;
  switch (op) {
  case Parser::NEG:
    value = -expr.value();
    break;
  case Parser::NOT:
    value = !expr.value();
    break;
  default:
    UNREACHABLE();
  }
  return Parser::Expression::make_value(value);
}
