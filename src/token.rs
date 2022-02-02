use crate::ast::BinOpType;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    /// Integer literal (64-bit)
    I64(i64),

    /// Identifier (name for variables, functions, ...)
    Ident(String),

    /// Loop keyword (loop)
    Loop,

    /// Print keyword (print)
    Print,

    /// If keyword (if)
    If,

    /// Else keyword (else)
    Else,

    /// Left Parenthesis ('(')
    LParen,

    /// Right Parenthesis (')')
    RParen,

    /// Left curly braces ({)
    LBraces,

    /// Right curly braces (})
    RBraces,

    /// Plus (+)
    Add,

    /// Minus (-)
    Sub,

    /// Asterisk (*)
    Mul,

    /// Slash (/)
    Div,

    /// Percent (%)
    Mod,

    /// Equal Equal (==)
    EquEqu,

    /// Exclamationmark Equal (!=)
    NotEqu,

    /// Pipe (|)
    BOr,

    /// Ampersand (&)
    BAnd,

    /// Circumflex (^)
    BXor,

    /// Logical AND (&&)
    LAnd,

    /// Logical OR (||)
    LOr,

    /// Shift Left (<<)
    Shl,

    /// Shift Right (>>)
    Shr,

    /// Tilde (~)
    Tilde,

    /// Logical not (!)
    LNot,

    /// Left angle bracket (<)
    LAngle,

    /// Right angle bracket (>)
    RAngle,

    /// Left angle bracket Equal (<=)
    LAngleEqu,

    /// Left angle bracket Equal (>=)
    RAngleEqu,

    /// Left arrow (<-)
    LArrow,

    /// Equal Sign (=)
    Equ,

    /// Semicolon (;)
    Semicolon,

    /// End of file
    EoF,
}

impl Token {
    pub fn try_to_binop(&self) -> Option<BinOpType> {
        Some(match self {
            Token::Add => BinOpType::Add,
            Token::Sub => BinOpType::Sub,

            Token::Mul => BinOpType::Mul,
            Token::Div => BinOpType::Div,
            Token::Mod => BinOpType::Mod,

            Token::BAnd => BinOpType::BAnd,
            Token::BOr => BinOpType::BOr,
            Token::BXor => BinOpType::BXor,

            Token::LAnd => BinOpType::LAnd,
            Token::LOr => BinOpType::LOr,

            Token::Shl => BinOpType::Shl,
            Token::Shr => BinOpType::Shr,

            Token::EquEqu => BinOpType::EquEqu,
            Token::NotEqu => BinOpType::NotEqu,

            Token::LAngle => BinOpType::Less,
            Token::LAngleEqu => BinOpType::LessEqu,

            Token::RAngle => BinOpType::Greater,
            Token::RAngleEqu => BinOpType::GreaterEqu,

            Token::LArrow => BinOpType::Declare,
            Token::Equ => BinOpType::Assign,

            _ => return None,
        })
    }
}
