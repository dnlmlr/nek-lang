use crate::{ast::{BinOpType, UnOpType}, T};

/// Language keywords
#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    /// Loop keyword ("loop")
    Loop,
    /// Print keyword ("print")
    Print,
    /// If keyword ("if")
    If,
    /// Else keyword ("else")
    Else,
}

/// Literal values
#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    /// Integer literal (64-bit)
    I64(i64),
    /// String literal
    String(String),
}

/// Combined tokens that consist of a combination of characters
#[derive(Debug, PartialEq, Eq)]
pub enum Combo {
    /// Equal Equal ("==")
    Equal2,

    /// Exclamation mark Equal ("!=")
    ExclamationMarkEqual,

    /// Ampersand Ampersand ("&&")
    Ampersand2,

    /// Pipe Pipe ("||")
    Pipe2,

    /// LessThan LessThan ("<<")
    LessThan2,

    /// GreaterThan GreaterThan (">>")
    GreaterThan2,

    /// LessThan Equal ("<=")
    LessThanEqual,

    /// GreaterThan Equal (">=")
    GreaterThanEqual,

    /// LessThan Minus ("<-")
    LessThanMinus,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    /// Literal value token
    Literal(Literal),

    /// Keyword token
    Keyword(Keyword),

    /// Identifier (name for variables, functions, ...)
    Ident(String),

    /// Combined tokens consisting of multiple characters
    Combo(Combo),

    /// Equal Sign ("=")
    Equal,

    /// Semicolon (";")
    Semicolon,

    /// End of file
    EoF,

    /// Left Bracket ("[")
    LBracket,

    /// Right Bracket ("]")
    RBracket,

    /// Left Parenthesis ("(")
    LParen,

    /// Right Parenthesis (")"")
    RParen,

    /// Left curly braces ("{")
    LBraces,

    /// Right curly braces ("}")
    RBraces,

    /// Plus ("+")
    Plus,

    /// Minus ("-")
    Minus,

    /// Asterisk ("*")
    Asterisk,

    /// Slash ("/")
    Slash,

    /// Percent ("%")
    Percent,

    /// Pipe ("|")
    Pipe,

    /// Tilde ("~")
    Tilde,

    /// Logical not ("!")
    Exclamationmark,

    /// Left angle bracket ("<")
    LessThan,

    /// Right angle bracket (">")
    GreaterThan,

    /// Ampersand ("&")
    Ampersand,

    /// Circumflex ("^")
    Circumflex,
}

impl Token {
    /// If the Token can be used as a binary operation type, get the matching BinOpType. Otherwise
    /// return None.
    pub fn try_to_binop(&self) -> Option<BinOpType> {
        Some(match self {
            T![+] => BinOpType::Add,
            T![-] => BinOpType::Sub,

            T![*] => BinOpType::Mul,
            T![/] => BinOpType::Div,
            T![%] => BinOpType::Mod,

            T![&] => BinOpType::BAnd,
            T![|] => BinOpType::BOr,
            T![^] => BinOpType::BXor,

            T![&&] => BinOpType::LAnd,
            T![||] => BinOpType::LOr,

            T![<<] => BinOpType::Shl,
            T![>>] => BinOpType::Shr,

            T![==] => BinOpType::EquEqu,
            T![!=] => BinOpType::NotEqu,

            T![<] => BinOpType::Less,
            T![<=] => BinOpType::LessEqu,

            T![>] => BinOpType::Greater,
            T![>=] => BinOpType::GreaterEqu,

            T![=] => BinOpType::Assign,

            _ => return None,
        })
    }

    pub fn try_to_unop(&self) -> Option<UnOpType> {
        Some(match self {
            T![-] => UnOpType::Negate,
            T![!] => UnOpType::LNot,
            T![~] => UnOpType::BNot,

            _ => return None,
        })
    }
}

/// Macro to quickly create a token of the specified kind
#[macro_export]
macro_rules! T {
    // Keywords
    [loop] => {
        crate::token::Token::Keyword(crate::token::Keyword::Loop)
    };

    [print] => {
        crate::token::Token::Keyword(crate::token::Keyword::Print)
    };

    [if] => {
        crate::token::Token::Keyword(crate::token::Keyword::If)
    };

    [else] => {
        crate::token::Token::Keyword(crate::token::Keyword::Else)
    };

    // Literals
    [i64($($val:tt)*)] => {
        crate::token::Token::Literal(crate::token::Literal::I64($($val)*))
    };

    [str($($val:tt)*)] => {
        crate::token::Token::Literal(crate::token::Literal::String($($val)*))
    };

    // Ident
    [ident($($val:tt)*)] => {
        crate::token::Token::Ident($($val)*)
    };

    // Combo crate::token::Tokens
    [==] => {
        crate::token::Token::Combo(crate::token::Combo::Equal2)
    };

    [!=] => {
        crate::token::Token::Combo(crate::token::Combo::ExclamationMarkEqual)
    };

    [&&] => {
        crate::token::Token::Combo(crate::token::Combo::Ampersand2)
    };

    [||] => {
        crate::token::Token::Combo(crate::token::Combo::Pipe2)
    };

    [<<] => {
        crate::token::Token::Combo(crate::token::Combo::LessThan2)
    };

    [>>] => {
        crate::token::Token::Combo(crate::token::Combo::GreaterThan2)
    };

    [<=] => {
        crate::token::Token::Combo(crate::token::Combo::LessThanEqual)
    };

    [>=] => {
        crate::token::Token::Combo(crate::token::Combo::GreaterThanEqual)
    };

    [<-] => {
        crate::token::Token::Combo(crate::token::Combo::LessThanMinus)
    };

    // Normal Tokens
    [=] => {
        crate::token::Token::Equal
    };

    [;] => {
        crate::token::Token::Semicolon
    };

    [EoF] => {
        crate::token::Token::EoF
    };

    ['['] => {
        crate::token::Token::LBracket
    };

    [']'] => {
        crate::token::Token::RBracket
    };

    ['('] => {
        crate::token::Token::LParen
    };

    [')'] => {
        crate::token::Token::RParen
    };

    ['{'] => {
        crate::token::Token::LBraces
    };

    ['}'] => {
        crate::token::Token::RBraces
    };

    [+] => {
        crate::token::Token::Plus
    };

    [-] => {
        crate::token::Token::Minus
    };

    [*] => {
        crate::token::Token::Asterisk
    };

    [/] => {
        crate::token::Token::Slash
    };

    [%] => {
        crate::token::Token::Percent
    };

    [|] => {
        crate::token::Token::Pipe
    };

    [~] => {
        crate::token::Token::Tilde
    };

    [!] => {
        crate::token::Token::Exclamationmark
    };

    [<] => {
        crate::token::Token::LessThan
    };

    [>] => {
        crate::token::Token::GreaterThan
    };

    [&] => {
        crate::token::Token::Ampersand
    };

    [^] => {
        crate::token::Token::Circumflex
    };
}
