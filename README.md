# NEK-Lang

## High level Components

- [x] Lexer: Transforms text into Tokens
- [x] Parser: Transforms Tokens into Abstract Syntax Tree
- [x] Interpreter (tree-walk-interpreter): Walks the tree and evaluates the expressions / statements
- [ ] Abstract Syntax Tree Optimizer

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
  - [ ] Logical boolean operators
    - [ ] Equal `==`
    - [ ] Not equal `!=`
    - [ ] Greater than `>`
    - [ ] Less than `<`
    - [ ] Greater than or equal `>=`
    - [ ] Less than or equal `<=`
  - [x] Bitwise operators
    - [x] Bitwise AND `X&Y`
    - [x] Bitwise OR `X|Y`
    - [x] Bitwise XOR `X^Y`
    - [x] Bitwise NOT `~X`
    - [x] Bitwise left shift `X<<Y`
    - [x] Bitwise right shift `X>>Y`
- [ ] Variables
  - [ ] Declaration
  - [ ] Assignment
- [ ] Control flow
  - [ ] While loop `while X { ... }`
  - [ ] If else statement `if X { ... } else { ... }`
    - [ ] If Statement
    - [ ] Else statement
- [ ] Line comments `//`
- [ ] Strings
- [ ] IO Intrinsics
  - [ ] Print
  - [ ] ReadLine
