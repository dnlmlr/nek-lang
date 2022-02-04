use crate::token::Token;
use anyhow::Result;
use std::{iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexErr {
    #[error("Failed to parse '{0}' as i64")]
    NumericParse(String),

    #[error("Invalid escape character '\\{0}'")]
    InvalidStrEscape(char),

    #[error("Lexer encountered unexpected char: '{0}'")]
    UnexpectedChar(char),

    #[error("Missing closing string quote '\"'")]
    MissingClosingString,
}

/// Lex the provided code into a Token Buffer
pub fn lex(code: &str) -> Result<Vec<Token>, LexErr> {
    let mut lexer = Lexer::new(code);
    lexer.lex()
}

struct Lexer<'a> {
    /// The sourcecode text as an iterator over the chars
    code: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Self {
        let code = code.chars().peekable();
        Self { code }
    }

    fn lex(&mut self) -> Result<Vec<Token>, LexErr> {
        let mut tokens = Vec::new();

        loop {
            match self.next() {
                // Stop lexing at EOF
                '\0' => break,

                // Skip whitespace
                ' ' | '\t' | '\n' | '\r' => (),

                // Line comment. Consume every char until linefeed (next line)
                '/' if matches!(self.peek(), '/') => while !matches!(self.next(), '\n' | '\0') {},

                // Double character tokens
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
                '&' if matches!(self.peek(), '&') => {
                    self.next();
                    tokens.push(Token::LAnd);
                }
                '|' if matches!(self.peek(), '|') => {
                    self.next();
                    tokens.push(Token::LOr);
                }

                // Single character tokens
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
                '{' => tokens.push(Token::LBraces),
                '}' => tokens.push(Token::RBraces),
                '!' => tokens.push(Token::LNot),
                '[' => tokens.push(Token::LBracket),
                ']' => tokens.push(Token::RBracket),

                // Special tokens with variable length

                // Lex multiple characters together as numbers
                ch @ '0'..='9' => tokens.push(self.lex_number(ch)?),

                // Lex multiple characters together as a string
                '"' => tokens.push(self.lex_str()?),

                // Lex multiple characters together as identifier
                ch @ ('a'..='z' | 'A'..='Z' | '_') => tokens.push(self.lex_identifier(ch)?),

                ch => Err(LexErr::UnexpectedChar(ch))?,
            }
        }

        Ok(tokens)
    }

    /// Lex multiple characters as a number until encountering a non numeric digit. This includes
    /// the first character
    fn lex_number(&mut self, first_char: char) -> Result<Token, LexErr> {
        // String representation of the integer value
        let mut sval = String::from(first_char);

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

        // Try to convert the string representation of the value to i64
        let i64val = sval.parse().map_err(|_| LexErr::NumericParse(sval))?;
        Ok(Token::I64(i64val))
    }

    /// Lex characters as a string until encountering an unescaped closing doublequoute char '"'
    fn lex_str(&mut self) -> Result<Token, LexErr> {
        // Opening " was consumed in match

        let mut text = String::new();

        // Read all chars until encountering the closing "
        loop {
            match self.peek() {
                '"' => break,
                // If the end of file is reached while still waiting for '"', error out
                '\0' => Err(LexErr::MissingClosingString)?,
                _ => match self.next() {
                    // Backshlash indicates an escaped character
                    '\\' => match self.next() {
                        'n' => text.push('\n'),
                        'r' => text.push('\r'),
                        't' => text.push('\t'),
                        '\\' => text.push('\\'),
                        '"' => text.push('"'),
                        ch => Err(LexErr::InvalidStrEscape(ch))?,
                    },
                    // All other characters are simply appended to the string
                    ch => text.push(ch),
                },
            }
        }

        // Consume closing "
        self.next();

        Ok(Token::String(text))
    }

    /// Lex characters from the text as an identifier. This includes the first character passed in
    fn lex_identifier(&mut self, first_char: char) -> Result<Token, LexErr> {
        let mut ident = String::from(first_char);

        // Do as long as a next char exists and it is a valid char for an identifier
        loop {
            match self.peek() {
                // In the middle of an identifier numbers are also allowed
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    ident.push(self.next());
                }
                // Next char is not valid, so stop and finish the ident token
                _ => break,
            }
        }

        // Check for pre-defined keywords
        let token = match ident.as_str() {
            "loop" => Token::Loop,
            "print" => Token::Print,
            "if" => Token::If,
            "else" => Token::Else,

            // If it doesn't match a keyword, it is a normal identifier
            _ => Token::Ident(ident),
        };

        Ok(token)
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

        let actual = lex(code).unwrap();
        assert_eq!(expected, actual);
    }
}
