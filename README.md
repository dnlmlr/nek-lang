# NEK-Lang

## Variables
Currently all variables are global and completely unscoped. That means no matter where a variable is declared, it remains over the whole remaining runtime of the progam.

All variables are currently of type `i64` (64-bit signed integer)

### Declaration
- Declare and initialize a new variable
- Declaring a previously declared variable again is currently equivalent to an assignment
- Declaration is needed before assignment or other usage
- The variable name is on the left side of the `<-` operator
- The assigned value is on the right side and can be any expression
```
a <- 123;
```
Create a new variable named `a` and assign the value `123` to it.

### Assignment
- Assigning a value to a previously declared variable
- The variable name is on the left side of the `=` operator
- The assigned value is on the right side and can be any expression
```
a = 123;
```
The value `123` is assigned to the variable named `a`. `a` needs to be declared before this.

## Expressions
The operator precedence is the same order as in `C` for all implemented operators. 
Refer to the 
[C Operator Precedence Table](https://en.cppreference.com/w/c/language/operator_precedence) 
to see the different precedences.

### General
- Parentheses `(` and `)` can be used to modify evaluation oder just like in any other 
programming language.
- For example `(a + b) * c` will evaluate the addition before the multiplication, despite the multiplication having higher binding power

### Mathematical Operators
Supported mathematical operations:
- Addition `a + b`
- Subtraction `a - b`
- Multiplication `a * b`
- Division `a / b`
- Modulo `a % b`
- Negation `-a`

### Bitwise Operators
- And `a & b`
- Or `a | b`
- Xor `a ^ b`
- Bitshift left (by `b` bits) `a << b`
- Bitshift right (by `b` bits) `a >> b`
- "Bit flip" (One's complement) `~a`

### Logical Operators
The logical operators evaluate the operands as `false` if they are equal to `0` and `true` if they are not equal to `0`
- And `a && b`
- Or `a || b`
- Not `!a` (if `a` is equal to `0`, the result is `1`, otherwise the result is `0`)

### Equality & Relational Operators
The equality and relational operations result in `1` if the condition is evaluated as `true` and in `0` if the condition is evaluated as `false`.
- Equality `a == b`
- Inequality `a != b`
- Greater than `a > b`
- Greater or equal than `a >= b`
- Less than `a < b`
- Less or equal than `a <= b`

## Control-Flow
For conditions like in if or loops, every non zero value is equal to `true`, and `0` is `false`.

### Loop
- There is currently only the `loop` keyword that can act like a `while` with optional advancement (an expression that is executed after the loop body)
- The `loop` keyword is followed by the condition (an expression) without needing parentheses
- *Optional:* If there is a `;` after the condition, there must be another expression which is used as the advancement
- The loops body is wrapped in braces (`{ }`) just like in C/C++

```
// Print the numbers from 0 to 9

// Without advancement
i <- 0;
loop i < 10 {
  print i;
  i = i - 1;
}

// With advancement
k <- 0;
loop k < 10; k = k - 1 {
  print k;
}
```

### If / Else

- The language supports `if` and an optional `else`
- After the `if` keyword must be the deciding condition, parentheses are not needed
- The block *if-true* block is wrapped in braces (`{ }`)
- *Optional:* If there is an `else` after the *if-block*, there must be a following *if-false*, aka. else block
```
a <- 1;
b <- 2;
if a == b {
  // a is equal to b
  print 1;
} else {
  // a is not equal to b
  print 0;
}
```

## IO

### Print
Printing is implemented via the `print` keyword
- The `print` keyword is followed by an expression, the value of which will be printed to the terminal.
- Print currently automatically adds a linebreak
```
a <- 1;
print a; // Outputs `"1\n"` to the terminal
```

## Comments

### Line comments
Line comments can be initiated by using `//`
- Everything after `//` up to the end of the current line is ignored and not parsed
```
// This is a comment
```


# Feature Tracker

## High level Components

- [x] Lexer: Transforms text into Tokens
- [x] Parser: Transforms Tokens into Abstract Syntax Tree
- [x] Interpreter (tree-walk-interpreter): Walks the tree and evaluates the expressions / statements

## Language features

- [x] General expressions
  - [x] Arithmetic operations
    - [x] Addition `a + b`
    - [x] Subtraction `a - b`
    - [x] Multiplication `a * b`
    - [x] Division `a / b`
    - [x] Modulo `a % b
    - [x] Negate `-a`
  - [x] Parentheses `(a + b) * c`
  - [x] Logical boolean operators
    - [x] Equal `a == b`
    - [x] Not equal `a != b`
    - [x] Greater than `a > b`
    - [x] Less than `a < b`
    - [x] Greater than or equal `a >= b`
    - [x] Less than or equal `a <= b`
  - [x] Logical operators
    - [x] And `a && b`
    - [x] Or `a || b`
    - [x] Not `!a`
  - [x] Bitwise operators
    - [x] Bitwise AND `a & b`
    - [x] Bitwise OR `a | b`
    - [x] Bitwise XOR `a ^ b`
    - [x] Bitwise NOT `~a`
    - [x] Bitwise left shift `a << b`
    - [x] Bitwise right shift `a >> b`
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
- [x] Strings
- [x] IO Intrinsics
  - [x] Print

## Grammar

### Expressions
```
LITERAL = I64_LITERAL | STR_LITERAL
expr_primary = LITERAL | IDENT | "(" expr ")" | "-" expr_primary | "~" expr_primary
expr_mul = expr_primary (("*" | "/" | "%") expr_primary)*
expr_add = expr_mul (("+" | "-") expr_mul)*
expr_shift = expr_add ((">>" | "<<") expr_add)*
expr_rel = expr_shift ((">" | ">=" | "<" | "<=") expr_shift)*
expr_equ = expr_rel (("==" | "!=") expr_rel)*
expr_band = expr_equ ("&" expr_equ)*
expr_bxor = expr_band ("^" expr_band)*
expr_bor = expr_bxor ("|" expr_bxor)*
expr_land = expr_bor ("&&" expr_bor)*
expr_lor = expr_land ("||" expr_land)*
expr = expr_lor
```

### Statements
```
stmt_if = "if" expr "{" stmt* "}" ("else" "{" stmt* "}")?
stmt_loop = "loop" expr (";" expr)? "{" stmt* "}"
stmt_expr = expr ";"
stmt = stmt_expr | stmt_loop
```