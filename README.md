# NEK-Lang

## High level Components

- [x] Lexer: Transforms text into Tokens
- [x] Parser: Transforms Tokens into Abstract Syntax Tree
- [x] Interpreter (tree-walk-interpreter): Walks the tree and evaluates the expressions / statements
- [ ] Abstract Syntax Tree Optimizer

## Language features

- [x] Math expressions
  - [ ] Unary operators
    - [ ] Negate `-X`
  - [ ] Parentheses `(X+Y)*Z`
  - [ ] Logical boolean operators
- [ ] Variables
  - [ ] Declaration
  - [ ] Assignment
- [ ] While loop `while X { ... }`
- [ ] If else statement `if X { ... } else { ... }`
  - [ ] If Statement
  - [ ] Else statement
- [ ] Line comments `//`
- [ ] Strings
- [ ] IO Intrinsics
  - [ ] Print
  - [ ] ReadLine
