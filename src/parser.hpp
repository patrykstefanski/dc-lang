#ifndef PARSER_HPP
#define PARSER_HPP

#include <cstdint>
#include <unordered_map>
#include <utility>
#include <vector>

#include "instruction.hpp"
#include "lexer.hpp"

class Parser {
public:
  explicit Parser(Lexer lexer);
  Parser(const Parser&) = delete;
  void operator=(const Parser&) = delete;

  const std::vector<Instruction>& bytecode() const;
  const std::vector<std::int64_t>& constants() const;

  // Parses the source.
  // program -> { function }
  void parse();

private:
  enum Operator {
    // Commutative binary operators.
    ADD,
    MUL,
    EQ,
    NE,
    // Noncommutative binary operators.
    SUB,
    DIV,
    MOD,
    LT,
    LE,
    // Other operators without corresponding instructions.
    GT,
    GE,
    // Unary operators.
    NEG,
    NOT,
  };

  // These constraints must be satisfied, otherwise some methods will
  // emit wrong code.
  static_assert(MUL - ADD == Instruction::MULRR - Instruction::ADDRR);
  static_assert(EQ - ADD == Instruction::EQRR - Instruction::ADDRR);
  static_assert(NE - ADD == Instruction::NERR - Instruction::ADDRR);
  static_assert(MUL - ADD == Instruction::MULRI - Instruction::ADDRI);
  static_assert(EQ - ADD == Instruction::EQRI - Instruction::ADDRI);
  static_assert(NE - ADD == Instruction::NERI - Instruction::ADDRI);
  static_assert(DIV - SUB == Instruction::DIVRR - Instruction::SUBRR);
  static_assert(MOD - SUB == Instruction::MODRR - Instruction::SUBRR);
  static_assert(LT - SUB == Instruction::LTRR - Instruction::SUBRR);
  static_assert(LE - SUB == Instruction::LERR - Instruction::SUBRR);
  static_assert(DIV - SUB == Instruction::DIVRI - Instruction::SUBRI);
  static_assert(MOD - SUB == Instruction::MODRI - Instruction::SUBRI);
  static_assert(LT - SUB == Instruction::LTRI - Instruction::SUBRI);
  static_assert(LE - SUB == Instruction::LERI - Instruction::SUBRI);
  static_assert(DIV - SUB == Instruction::DIVIR - Instruction::SUBIR);
  static_assert(MOD - SUB == Instruction::MODIR - Instruction::SUBIR);
  static_assert(LT - SUB == Instruction::LTIR - Instruction::SUBIR);
  static_assert(LE - SUB == Instruction::LEIR - Instruction::SUBIR);
  static_assert(GE - GT == LE - LT);
  static_assert(NOT - NEG == Instruction::NOT - Instruction::NEG);

  struct Expression {
    Expression() : value_(0), has_reg_(false) {
    }

    Expression(const Expression&) = default;
    Expression& operator=(const Expression&) = default;

    // Makes a value expression.
    static Expression make_value(std::int64_t value) {
      Expression expr;
      expr.value_ = value;
      expr.has_reg_ = false;
      return expr;
    }

    // Makes an expression with a value.
    static Expression make_reg(std::uint8_t reg) {
      Expression expr;
      expr.reg_ = reg;
      expr.has_reg_ = true;
      return expr;
    }

    std::int64_t value() const {
      return value_;
    }

    std::uint8_t reg() const {
      return reg_;
    }

    bool has_reg() const {
      return has_reg_;
    }

  private:
    union {
      std::int64_t value_;
      std::uint8_t reg_;
    };
    bool has_reg_;
  };

  struct Scope {
    Scope() : first_free_reg_(0), num_variables_(0) {
    }

    Scope(const Scope&) = default;
    Scope& operator=(const Scope&) = default;

    std::size_t first_free_reg_;
    std::size_t num_variables_;
  };

  // Generates bytecode from source.
  void first_pass();

  // Checks and patches function calls.
  void second_pass();

  // Parsing.

  // Parses a function.
  // function -> FN IDENTIFIER '(' arguments ')' block
  void parse_fn();

  // Parses function arguments, returns the number of them.
  // arguments -> <none> | IDENTIFIER { ',' IDENTIFIER }
  std::size_t parse_arguments();

  // Parses a statement. Returns true iff we should parse more in the block.
  // statement -> block | if | while | let | array | del | return | in | out |
  //              assignment_or_call
  bool parse_statement();

  // Parses a block statement.
  // block -> '{' { statement } '}'
  void parse_block();

  // Parses an if statement.
  // if -> IF cond_block { ELSE IF cond_block } [ ELSE block ]
  void parse_if();

  // Parses a conditional block for IF / ELSE IF, returns the jump list
  // for false conditions.
  // cond_block -> expr block
  std::size_t parse_cond_block();

  // Parses a while statement.
  // while -> WHILE cond_block
  void parse_while();

  // Parses a let statement.
  // let -> LET IDENTIFIER '=' expr ';'
  void parse_let();

  // Parses an array statement.
  // array -> ARRAY IDENTIFIER '[' expr ']' ';'
  void parse_array();

  // Parses a del statement.
  // del -> DEL expr ';'
  void parse_del();

  // Parses a return statement.
  // return -> RETURN expr ';'
  void parse_return();

  // Parses an in statement.
  // in -> IN IDENTIFIER ';'
  void parse_in();

  // Parses an out statement.
  // out -> OUT expr ';'
  void parse_out();

  // Parses an assignment or a call.
  // assignment_or_call -> IDENTIFIER '=' expr ';' |
  //                       IDENTIFIER '(' parameters ')' ';'
  void parse_assignment_or_call();

  // Parses function parameters, returns the number of them.
  // parameters -> <none> | expr { ',' expr }
  std::size_t parse_parameters();

  // Parses an expression.
  // expr -> binary_expr
  Expression parse_expr();

  // Parses a binary expression with higher precedence than limit.
  // binary_expr -> unary_expr [ ( '+', '-', ...) binary_expr ]
  Expression parse_binary_expr(std::size_t limit);

  // Parses an unary expression.
  // unary_expr -> [ '!' | '-' ] primary_expr
  Expression parse_unary_expr();

  // Parses a primary expression.
  // primary_expr -> identifier_expr | integer_literal_expr | parentheses_expr
  Expression parse_primary_expr();

  // Parses a parentheses expression.
  // parentheses_expr -> '(' expr ')'
  Expression parse_parentheses_expr();

  // Parses an identifier expression. It can be a variable or a function call.
  // identifier_expr -> IDENTIFIER [ '(' parameters ')' ]
  Expression parse_identifier_expr();

  // Parses an integer literal expression.
  // integer_literal_expr -> INTEGER_LITERAL
  Expression parse_integer_literal_expr();

  // Variables and registers.

  // Finds the register where the given variable (symbol_id) is stored.
  // Returns the variable register if the variable exists in the current
  // scope, std::numeric_limits<std::size_t>::max() otherwise.
  std::size_t find_variable_reg(std::size_t symbol_id) const;

  // Stores the expression into the given register.
  Expression expr_to_reg(Expression expr, std::uint8_t reg);

  // Stores the expression into the next free register.
  Expression expr_to_next_reg(Expression expr);

  // Stores the expression into any register.
  Expression expr_to_any_reg(Expression expr);

  // Tries to free the expression register if any. It won't free any register
  // that is used by a variable.
  void free_expr_reg(Expression expr);

  // Jump lists.

  // Gets the next jump.
  std::size_t next_jump(std::size_t pos) const;

  // Appends the position in the bytecode to the jump list.
  void append_jump(std::size_t* list, std::size_t pos);

  // Patches the single instruction at given position to the target.
  void patch_single_jump(std::size_t pos, std::size_t target);

  // Patches the jump list to the given target.
  void patch_jump_list(std::size_t list, std::size_t target);

  // Patches the jump list to the current program counter.
  void patch_jump_list_to_here(std::size_t list);

  // Bytecode emitting.

  // Emits a binary operator. None will be emitted if the both expressions can
  // be folded.
  Expression emit_binary_op(int op, Expression lhs, Expression rhs);

  // Emits a commutative binary operator.
  Expression emit_commutative_op(int op, Expression lhs, Expression rhs);

  // Emits a noncommutative binary operator.
  Expression emit_noncommutative_op(int op, Expression lhs, Expression rhs);

  // Emits an unary operator.
  Expression emit_unary_op(int op, Expression expr);

  // Emits an unconditional jump.
  std::size_t emit_unconditional_jump();

  // Emits a conditional jump. The 'reg' register will be compared.
  // If confition is true, then JT will be generated, JF otherwise.
  std::size_t emit_conditional_jump(std::uint8_t reg, bool condition);

  // Emits a call instruction.
  Expression emit_call(std::size_t symbol_id, std::uint8_t reg,
                       std::size_t num_args);

  // Emits a return instruction.
  void emit_return(Expression expr);

  // Emits an array new instruction.
  void emit_array_new(Expression size_expr);

  // Emits an array delete instruction.
  void emit_array_delete(Expression expr);

  // Emits an array get instruction and returns the expression that refers to
  // the value at specified index.
  Expression emit_array_get(std::uint8_t array_reg, Expression index_expr);

  // Emits an array set instruction.
  void emit_array_set(std::uint8_t array_reg, Expression index_expr,
                      Expression value_expr);

  // Emits an in/out instruction.
  void emit_io(Instruction::Opcode, std::uint8_t reg);

  // Utilities.

  // Converts token to operator, which can be used internally.
  static int token_to_binary_op(int token);

  // Folds a binary operator.
  static Expression fold_binary_op(int op, Expression lhs, Expression rhs);

  // Folds a binary operator.
  static Expression fold_unary_op(int op, Expression expr);

  std::size_t variable_regs_[0xff];
  // Map of defined functions with their positions in the bytecode and number
  // of arguments.
  std::unordered_map<std::size_t, std::pair<std::size_t, std::size_t>>
      functions_;
  Lexer lexer_;
  std::vector<Instruction> bytecode_;
  std::vector<std::int64_t> constants_;
  Scope* current_scope_;
};

#endif  // !PARSER_HPP
