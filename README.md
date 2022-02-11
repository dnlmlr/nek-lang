# NEK-Lang
## Table of contents
- [NEK-Lang](#nek-lang)
  - [Table of contents](#table-of-contents)
  - [Variables](#variables)
    - [Declaration](#declaration)
    - [Assignment](#assignment)
  - [Datatypes](#datatypes)
    - [I64](#i64)
    - [String](#string)
    - [Array](#array)
  - [Expressions](#expressions)
    - [General](#general)
    - [Mathematical Operators](#mathematical-operators)
    - [Bitwise Operators](#bitwise-operators)
    - [Logical Operators](#logical-operators)
    - [Equality & Relational Operators](#equality--relational-operators)
  - [Control-Flow](#control-flow)
    - [Loop](#loop)
    - [If / Else](#if--else)
    - [Block Scopes](#block-scopes)
  - [Functions](#functions)
    - [Function definition](#function-definition)
    - [Function calls](#function-calls)
  - [IO](#io)
    - [Print](#print)
  - [Comments](#comments)
    - [Line comments](#line-comments)
- [Feature Tracker](#feature-tracker)
  - [High level Components](#high-level-components)
  - [Language features](#language-features)
- [Parsing Grammar](#parsing-grammar)
  - [Expressions](#expressions-1)
  - [Statements](#statements)
- [Examples](#examples)

## Variables
The variables are all contained in scopes. Variables defined in an outer scope can be accessed in 
inner scoped. All variables defined in a scope that has ended do no longer exist and can't be 
accessed.

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

## Datatypes
The available variable datatypes are `i64` (64-bit signed integer), `string` (`"this is a string"`) and `array` (`[10]`)

### I64
- The normal default datatype is `i64` which is a 64-bit signed integer
- Can be created by just writing an integer literal like `546`
- Inside the number literal `_` can be inserted for visual separation `100_000`
- The i64 values can be used as expected in calculations, conditions and so on
- 
```
my_i64 <- 123_456;
```

### String
- Strings mainly exist for formatting the text output of a program
- Strings can be created by using doublequotes like in other languages `"Hello world"`
- There is no way to access or change the characters of the string
- Unicode characters are supported `"Hello 🌎"`
- Escape characters `\n`, `\r`, `\t`, `\"`, `\\` are supported
- String can still be assigned to variables, just like i64
```
world <- "🌎";

print "Hello ";
print world;
print "\n";
```

### Array
- Arrays can contain any other datatypes and don't need to have the same type in all cells
- Arrays can be created by using brackets with the size in between `[size]`
- Arrays must be assigned to a variable to be used
- All cells will be initialized with i64 0 values
- The size can be any expression that results in a positive i64 value
- The array size can't be changed after creation
- The arrays data is always allocated on the heap
- The array cells can be accessed by using the variable name and brackets `my_arr[index]`
- The index can be any expression that results in a positive i64 value in the range of the arrays 
  indices
- The indices start with 0
- When an array is passed to a function, it is passed by reference
```
width <- 5;
heigt <- 5;

// Initialize array of size 25 with 25x 0
my_array = [width * height];

// Modify first value
my_array[0] = 5;

// Print first value
print my_array[0];
```

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
- The `loop` keyword can be used as an infinite loop, as a while loop or as a while loop with advancement (an expression that is executed after the loop body)
- If only `loop` is used, directly followed by the body, it is an infinite loop that needs to be 
  terminated by using the `break` keyword
- The `loop` keyword is followed by the condition (an expression) without needing parentheses
- *Optional:* If there is a `;` after the condition, there must be another expression which is used as the advancement
- The loops body is wrapped in braces (`{ }`) just like in C/C++
- The `continue` keyword can be used to end the current loop iteration early
- The `break` keyword can be used to fully break out of the current loop

```
// Print the numbers from 0 to 9

// With endless loop
i <- 0;
loop {
  if i >= 10 {
    break;
  }
  print i;
  i = i + 1;
}

// Without advancement
i <- 0;
loop i < 10 {
  print i;
  i = i + 1;
}

// With advancement
k <- 0;
loop k < 10; k = k + 1 {
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

### Block Scopes

- It is possible to create a limited scope for local variables that will no longer exist once the 
  scope ends
- Shadowing variables by redefining a variable in an inner scope is supported
```
var_in_outer_scope <- 5;
{
  var_in_inner_scope <- 3;
  
  // Inner scope can access both vars
  print var_in_outer_scope;
  print var_in_inner_scope;
}

// Outer scope is still valid
print var_in_outer_scope;

// !!! THIS DOES NOT WORK !!!
// The inner scope has ended
print var_in_inner_scope;
```

## Functions

### Function definition
- Functions can be defined by using the `fun` keyword, followed by the function name and the 
  parameters in parentheses. After the parentheses, the body is specified inside a braces block
- The function parameters are specified by only the names
- The function body has its own scope
- Parameters are only accessible inside the body
- Variables from the outer scope can be accessed and modified if the are defined before the function
- Variables from the outer scope are shadowed by parameters with the same name
- The `return` keyword can be used to return a value from the function and exit it immediately
- If no return is specified, a `void` value is returned
- Functions can only be defined at the top-level. So defining a function inside of any other scoped 
  block (like inside another function, if, loop, ...) is invalid
- Functions can only be used after definition and there is no forward declaration right now
- However a function can be called recursively inside of itself
- Functions can't be redefined, so defining a function with an existing name is invalid
```
fun add_maybe(a, b) {
  if a < 100 {
    return a;
  } else {
    return a + b;
  }
}

fun println(val) {
  print val;
  print "\n";
}
```

### Function calls
- Function calls are primary expressions, so they can be directly used in calculations (if they 
  return appropriate values)
- Function calls are performed by writing the function name, followed by the arguments in parentheses
- The arguments can be any expressions, separated by commas
```
b <- 100;
result <- add_maybe(250, b);

// Prints 350 + new-line
println(result);
```

## IO

### Print
Printing is implemented via the `print` keyword
- The `print` keyword is followed by an expression, the value of which will be printed to the terminal.
- Print currently automatically adds a linebreak
```
a <- 1;
// Outputs `"1"` to the terminal
print a;
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
- [x] Simple optimizer: Apply trivial optimizations to the Ast
  - [x] Precalculate binary ops / unary ops that have only literal operands

## Language features

- [x] General expressions
  - [x] Arithmetic operations
    - [x] Addition `a + b`
    - [x] Subtraction `a - b`
    - [x] Multiplication `a * b`
    - [x] Division `a / b`
    - [x] Modulo `a % b`
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
  - [x] Local variables (for example inside loop, if, else, functions)
  - [x] Scoped block for specific local vars `{ ... }`
- [x] Statements with semicolon & Multiline programs
- [x] Control flow
  - [x] Loops
    - [x] While-style loop `loop X { ... }`
    - [x] For-style loop without with `X` as condition and `Y` as advancement `loop X; Y { ... }`
    - [x] Infinite loop `loop { ... }`
    - [x] Break `break`
    - [x] Continue `continue`
  - [x] If else statement `if X { ... } else { ... }`
    - [x] If Statement
    - [x] Else statement
- [x] Line comments `//`
- [x] Strings
- [x] Arrays
  - [x] Creating array with size `X` as a variable `arr <- [X]`
  - [x] Accessing arrays by index `arr[X]`
- [x] IO Intrinsics
  - [x] Print
- [x] Functions
  - [x] Function declaration `fun f(X, Y, Z) { ... }`
  - [x] Function calls `f(1, 2, 3)`
  - [x] Function returns `return X`
  - [x] Local variables
  - [x] Pass arrays by-reference, i64 by-vale, string is a const ref

# Parsing Grammar

## Expressions
```
ARRAY_LITERAL = "[" expr "]"
ARRAY_ACCESS = IDENT "[" expr "]"
FUN_CALL = IDENT "(" (expr ",")* expr? ")"
LITERAL = I64_LITERAL | STR_LITERAL | ARRAY_LITERAL
expr_primary = LITERAL | IDENT | FUN_CALL | ARRAY_ACCESS | "(" expr ")" | "-" expr_primary 
             | "~" expr_primary
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

## Statements
```
stmt_return = "return" expr ";"
stmt_break = "break" ";"
stmt_continue = "continue" ";"
stmt_var_decl = IDENT "<-" expr ";"
stmt_fun_decl = "fun" IDENT "(" (IDENT ",")* IDENT? ")" "{" stmt* "}"
stmt_expr = expr ";"
stmt_block = "{" stmt* "}"
stmt_loop = "loop" (expr (";" expr)?)? "{" stmt* "}"
stmt_if = "if" expr "{" stmt* "}" ("else" "{" stmt* "}")?
stmt_print = "print" expr ";"
stmt = stmt_return | stmt_break | stmt_continue | stmt_var_decl | stmt_fun_decl 
     | stmt_expr | stmt_block | stmt_loop | stmt_if | stmt_print
```

# Examples
There are a bunch of examples in the [examples](examples/) directory. Those include (non-optimal) solutions to the first five project euler problems, as well as a [simple Game of Life implementation](examples/game_of_life.nek).

To run an example via `cargo-run`, use:
```
cargo run --release -- examples/[NAME]
```
