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
    /// Variable
    Var(String),
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
    pub body: Ast,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    /// The condition
    pub condition: Expression,
    /// The body that is executed when condition is true
    pub body_true: Ast,
    /// The if body that is executed when the condition is false
    pub body_false: Ast,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Expr(Expression),
    Loop(Loop),
    If(If),
    Print(Expression),
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Ast {
    pub prog: Vec<Statement>,
}
