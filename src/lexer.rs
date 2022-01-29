use std::{iter::Peekable, str::Chars};

use crate::parser::BinOpType;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    /// Integer literal (64-bit)
    I64(i64),

    /// Identifier (name for variables, functions, ...)
    Ident(String),

    /// Left Parenthesis ('(')
    LParen,

    /// Right Parenthesis (')')
    RParen,

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

    /// Shift Left (<<)
    Shl,

    /// Shift Right (>>)
    Shr,

    /// Tilde (~)
    Tilde,

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

struct Lexer<'a> {
    code: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Self {
        let code = code.chars().peekable();
        Self { code }
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            match self.next() {
                // Skip whitespace
                ' ' | '\t' | '\n' | '\r' => (),

                // Stop lexing at EOF
                '\0' => break,

                '>' if matches!(self.peek(), '>') => {
                    self.next();
                    tokens.push(Token::Shr);
                }
                '<' if matches!(self.peek(), '<') => {
                    self.next();
                    tokens.push(Token::Shl);
                }
                '=' if matches!(self.peek(), '=') => {
                    self.next();
                    tokens.push(Token::EquEqu);
                }
                '!' if matches!(self.peek(), '=') => {
                    self.next();
                    tokens.push(Token::NotEqu);
                }
                '<' if matches!(self.peek(), '=') => {
                    self.next();
                    tokens.push(Token::LAngleEqu);
                }
                '>' if matches!(self.peek(), '=') => {
                    self.next();
                    tokens.push(Token::RAngleEqu);
                }
                '<' if matches!(self.peek(), '-') => {
                    self.next();
                    tokens.push(Token::LArrow);
                }
                // Line comment. Consume every char until linefeed (next line)
                '/' if matches!(self.peek(), '/') => while self.next() != '\n' {},

                ';' => tokens.push(Token::Semicolon),
                '+' => tokens.push(Token::Add),
                '-' => tokens.push(Token::Sub),
                '*' => tokens.push(Token::Mul),
                '/' => tokens.push(Token::Div),
                '%' => tokens.push(Token::Mod),
                '|' => tokens.push(Token::BOr),
                '&' => tokens.push(Token::BAnd),
                '^' => tokens.push(Token::BXor),
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '~' => tokens.push(Token::Tilde),
                '<' => tokens.push(Token::LAngle),
                '>' => tokens.push(Token::RAngle),
                '=' => tokens.push(Token::Equ),

                // Lex numbers
                ch @ '0'..='9' => {
                    let mut sval = String::from(ch);

                    // Do as long as a next char exists and it is a numeric char
                    loop {
                        // The next char is verified to be Some, so unwrap is safe
                        match self.peek() {
                            // Underscore is a separator, so remove it but don't add to number
                            '_' => {
                                self.next();
                            }
                            '0'..='9' => {
                                sval.push(self.next());
                            }
                            // Next char is not a number, so stop and finish the number token
                            _ => break,
                        }
                    }

                    // TODO: We only added numeric chars to the string, but the conversion could still fail
                    tokens.push(Token::I64(sval.parse().unwrap()));
                }

                // Lex characters as identifier
                ch @ ('a'..='z' | 'A'..='Z' | '_') => {
                    let mut ident = String::from(ch);

                    // Do as long as a next char exists and it is a valid char for an identifier
                    loop {
                        match self.peek() {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                ident.push(self.next());
                            }
                            // Next char is not valid, so stop and finish the ident token
                            _ => break,
                        }
                    }

                    tokens.push(Token::Ident(ident));
                }

                //TODO: Don't panic, keep calm
                ch => panic!("Lexer encountered unexpected char: '{}'", ch),
            }
        }

        tokens
    }

    /// Advance to next character and return the removed char
    fn next(&mut self) -> char {
        self.code.next().unwrap_or('\0')
    }

    /// Get the next character without removing it
    fn peek(&mut self) -> char {
        self.code.peek().copied().unwrap_or('\0')
    }
}

/// Lex the provided code into a Token Buffer
///
/// TODO: Don't panic and implement error handling using Result
pub fn lex(code: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(code);
    lexer.lex()
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

#[cfg(test)]
mod tests {
    use super::{lex, Token};

    #[test]
    fn test_lexer() {
        let code = "33   +5*2  + 4456467*2334+3 % - / << ^ | & >>";
        let expected = vec![
            Token::I64(33),
            Token::Add,
            Token::I64(5),
            Token::Mul,
            Token::I64(2),
            Token::Add,
            Token::I64(4456467),
            Token::Mul,
            Token::I64(2334),
            Token::Add,
            Token::I64(3),
            Token::Mod,
            Token::Sub,
            Token::Div,
            Token::Shl,
            Token::BXor,
            Token::BOr,
            Token::BAnd,
            Token::Shr,
        ];

        let actual = lex(code);
        assert_eq!(expected, actual);
    }
}
