use crate::stringstore::{StringStore, Sid};

/// Types for binary operators
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOpType {
    /// Addition
    Add,

    /// Subtraction
    Sub,

    /// Multiplication
    Mul,

    /// Divide
    Div,

    /// Modulo
    Mod,

    /// Compare Equal
    EquEqu,

    /// Compare Not Equal
    NotEqu,

    /// Less than
    Less,

    /// Less than or Equal
    LessEqu,

    /// Greater than
    Greater,

    /// Greater than or Equal
    GreaterEqu,

    /// Bitwise OR (inclusive or)
    BOr,

    /// Bitwise And
    BAnd,

    /// Bitwise Xor (exclusive or)
    BXor,

    /// Logical And
    LAnd,

    /// Logical Or
    LOr,

    /// Shift Left
    Shl,

    /// Shift Right
    Shr,

    /// Assign value to variable
    Assign,

    /// Declare new variable with value
    Declare,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnOpType {
    /// Unary Negate
    Negate,

    /// Bitwise Not
    BNot,

    /// Logical Not
    LNot,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    /// Integer literal (64-bit)
    I64(i64),
    /// String literal
    String(Sid),
    /// Variable
    Var(Sid, usize),
    /// Binary operation. Consists of type, left hand side and right hand side
    BinOp(BinOpType, Box<Expression>, Box<Expression>),
    /// Unary operation. Consists of type and operand
    UnOp(UnOpType, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Loop {
    /// The condition that determines if the loop should continue
    pub condition: Expression,
    /// This is executed after each loop to advance the condition variables
    pub advancement: Option<Expression>,
    /// The loop body that is executed each loop
    pub body: BlockScope,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    /// The condition
    pub condition: Expression,
    /// The body that is executed when condition is true
    pub body_true: BlockScope,
    /// The if body that is executed when the condition is false
    pub body_false: BlockScope,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Expr(Expression),
    Block(BlockScope),
    Loop(Loop),
    If(If),
    Print(Expression),
}

pub type BlockScope = Vec<Statement>;

#[derive(Clone, Default)]
pub struct Ast {
    pub stringstore: StringStore,
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
            BinOpType::Declare => 0,
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
