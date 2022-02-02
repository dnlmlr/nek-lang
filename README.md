# NEK-Lang

## High level Components

- [x] Lexer: Transforms text into Tokens
- [x] Parser: Transforms Tokens into Abstract Syntax Tree
- [x] Interpreter (tree-walk-interpreter): Walks the tree and evaluates the expressions / statements

## Language features

- [x] General expressions
  - [x] Arithmetic operations
    - [x] Addition `X+Y`
    - [x] Subtraction `X-Y`
    - [x] Multiplication `X*Y`
    - [x] Division `X/Y`
    - [x] Modulo `X%Y`
  - [x] Unary operators
    - [x] Negate `-X`
  - [x] Parentheses `(X+Y)*Z`
  - [x] Logical boolean operators
    - [x] Equal `==`
    - [x] Not equal `!=`
    - [x] Greater than `>`
    - [x] Less than `<`
    - [x] Greater than or equal `>=`
    - [x] Less than or equal `<=`
  - [x] Bitwise operators
    - [x] Bitwise AND `X&Y`
    - [x] Bitwise OR `X|Y`
    - [x] Bitwise XOR `X^Y`
    - [x] Bitwise NOT `~X`
    - [x] Bitwise left shift `X<<Y`
    - [x] Bitwise right shift `X>>Y`
- [x] Variables
  - [x] Declaration
  - [x] Assignment
- [x] Statements with semicolon & Multiline programs
- [x] Control flow
  - [x] While loop `while X { ... }`
  - [x] If else statement `if X { ... } else { ... }`
    - [x] If Statement
    - [x] Else statement
- [x] Line comments `//`
- [ ] Strings
- [ ] IO Intrinsics
  - [x] Print
  - [ ] ReadInt

## Grammar

### Expressions
```
expr_primary = LITERAL | IDENT | "(" expr ")" | "-" expr_primary | "~" expr_primary
expr_mul = expr_primary (("*" | "/" | "%") expr_primary)*
expr_add = expr_mul (("+" | "-") expr_mul)*
expr_shift = expr_add ((">>" | "<<") expr_add)*
expr_rel = expr_shift ((">" | ">=" | "<" | "<=") expr_shift)*
expr_equ = expr_rel (("==" | "!=") expr_rel)*
expr_band = expr_equ ("&" expr_equ)*
expr_bxor = expr_band ("^" expr_band)*
expr_bor = expr_bxor ("|" expr_bxor)*
expr = expr_bor
```

### Statements
```
stmt_if = "if" expr "{" stmt* "}" ("else" "{" stmt* "}")?
stmt_loop = "loop" expr (";" expr)? "{" stmt* "}"
stmt_expr = expr ";"
stmt = stmt_expr | stmt_loop
```