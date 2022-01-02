use std::{iter::Peekable, str::Chars};

use crate::parser::BinOpType;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    /// Integer literal (64-bit)
    I64(i64),

    /// Plus (+)
    Add,

    /// Asterisk (*)
    Mul,

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

        while let Some(ch) = self.next() {
            match ch {
                // Skip whitespace
                ' ' => (),

                // Lex numbers
                '0'..='9' => {
                    let mut sval = String::from(ch);

                    // Do as long as a next char exists and it is a numeric char
                    while let Some('0'..='9') = self.peek() {
                        // The next char is verified to be Some, so unwrap is safe
                        sval.push(self.next().unwrap());
                    }

                    // TODO: We only added numeric chars to the string, but the conversion could still fail
                    tokens.push(Token::I64(sval.parse().unwrap()));
                }

                '+' => tokens.push(Token::Add),
                '*' => tokens.push(Token::Mul),

                //TODO: Don't panic, keep calm
                _ => panic!("Lexer encountered unexpected char: '{}'", ch),
            }
        }

        tokens
    }

    /// Advance to next character and return the removed char
    fn next(&mut self) -> Option<char> {
        self.code.next()
    }

    /// Get the next character without removing it
    fn peek(&mut self) -> Option<char> {
        self.code.peek().copied()
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
            Token::Mul => BinOpType::Mul,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{lex, Token};

    #[test]
    fn test_lexer() {
        let code = "33   +5*2  + 4456467*2334+3";
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
        ];

        let actual = lex(code);
        assert_eq!(expected, actual);
    }
}
