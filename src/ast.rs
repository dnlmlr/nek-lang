use std::rc::Rc;

use crate::stringstore::{Sid, StringStore};

/// Types for binary operations
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOpType {
    /// Addition ("+")
    Add,

    /// Subtraction ("-")
    Sub,

    /// Multiplication ("*")
    Mul,

    /// Division ("/")
    Div,

    /// Modulo / Remainder ("%")
    Mod,

    /// Compare Equal ("==")
    EquEqu,

    /// Compare Not Equal ("!=")
    NotEqu,

    /// Compare Less than ("<")
    Less,

    /// Compare Less than or Equal ("<=")
    LessEqu,

    /// Compare Greater than (">")
    Greater,

    /// Compare Greater than or Equal (">=")
    GreaterEqu,

    /// Bitwise Or ("|")
    BOr,

    /// Bitwise And ("&")
    BAnd,

    /// Bitwise Xor / Exclusive Or ("^")
    BXor,

    /// Logical And ("&&")
    LAnd,

    /// Logical Or ("||")
    LOr,

    /// Bitwise Shift Left ("<<")
    Shl,

    /// Bitwise Shift Right (">>")
    Shr,

    /// Assign value to variable ("=")
    Assign,
}

/// Types for unary operations
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnOpType {
    /// Unary Negation ("-")
    Negate,

    /// Bitwise Not / Bitflip ("~")
    BNot,

    /// Logical Not ("!")
    LNot,
}

/// Ast Node for possible Expression variants
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    /// Integer literal (64-bit)
    I64(i64),
    /// String literal
    String(Sid),

    /// Array with size as an expression
    ArrayLiteral(Box<Expression>),
    /// Array access with name, stackpos and position as expression
    ArrayAccess(Sid, usize, Box<Expression>),

    /// Function call with name, stackpos and the arguments as a vec of expressions
    FunCall(Sid, usize, Vec<Expression>),

    /// Variable with name and the stackpos from behind. This means that stackpos 0 refers to the 
    /// last variable on the stack and not the first
    Var(Sid, usize),
    /// Binary operation. Consists of type, left hand side and right hand side
    BinOp(BinOpType, Box<Expression>, Box<Expression>),
    /// Unary operation. Consists of type and operand
    UnOp(UnOpType, Box<Expression>),
}

/// Ast Node for a loop
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Loop {
    /// The condition that determines if the loop should continue
    pub condition: Option<Expression>,
    /// This is executed after each loop to advance the condition variables
    pub advancement: Option<Expression>,
    /// The loop body that is executed each loop
    pub body: BlockScope,
}

/// Ast Node for an if
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    /// The condition
    pub condition: Expression,
    /// The body that is executed when condition is true
    pub body_true: BlockScope,
    /// The if body that is executed when the condition is false
    pub body_false: BlockScope,
}

/// Ast Node for a function declaration
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunDecl {
    /// The function name as StringID, stored in the stringstore
    pub name: Sid,
    /// The absolute position on the function stack where the function is stored
    pub fun_stackpos: usize,
    /// The argument names as StringIDs
    pub argnames: Vec<Sid>,
    /// The function body
    pub body: Rc<BlockScope>,
}

/// Ast Node for a variable declaration
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDecl {
    /// The variable name as StringID, stored in the stringstore
    pub name: Sid,
    /// The absolute position on the variable stack where the variable is stored
    pub var_stackpos: usize,
    /// The right hand side that generates the initial value for the variable
    pub rhs: Expression,
}

/// Ast Node for the possible Statement variants
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    /// Return from a function with the given result value as an expression
    Return(Expression),
    /// Break out of the current loop
    Break,
    /// End the current loop iteration early and continue with the next loop iteration
    Continue,
    /// A variable declaration
    Declaration(VarDecl),
    /// A function declaration
    FunDeclare(FunDecl),
    /// A simple expression. This could be a function call or an assignment for example
    Expr(Expression),
    /// A freestanding block scope
    Block(BlockScope),
    /// A loop
    Loop(Loop),
    /// An if
    If(If),
    /// A print statement that will output the value of the given expression to the terminal
    Print(Expression),
}

/// A number of statements that form a block of code together
pub type BlockScope = Vec<Statement>;

/// A full abstract syntax tree
#[derive(Clone, Default)]
pub struct Ast {
    /// The stringstore contains the actual string values which are replaced with StringIDs in the
    /// Ast. So this is needed to get the actual strings later
    pub stringstore: StringStore,
    /// The main (top-level) code given as a number of statements
    pub main: BlockScope,
}

impl BinOpType {
    /// Get the precedence for a binary operator. Higher value means the OP is stronger binding.
    /// For example Multiplication is stronger than addition, so Mul has higher precedence than Add.
    ///
    /// The operator precedences are derived from the C language operator precedences. While not all
    /// C operators are included or the exact same, the precedence oder is the same.
    /// See: https://en.cppreference.com/w/c/language/operator_precedence

    pub fn precedence(&self) -> u8 {
        match self {
            BinOpType::Assign => 1,
            BinOpType::LOr => 2,
            BinOpType::LAnd => 3,
            BinOpType::BOr => 4,
            BinOpType::BXor => 5,
            BinOpType::BAnd => 6,
            BinOpType::EquEqu | BinOpType::NotEqu => 7,
            BinOpType::Less | BinOpType::LessEqu | BinOpType::Greater | BinOpType::GreaterEqu => 8,
            BinOpType::Shl | BinOpType::Shr => 9,
            BinOpType::Add | BinOpType::Sub => 10,
            BinOpType::Mul | BinOpType::Div | BinOpType::Mod => 11,
        }
    }
}
