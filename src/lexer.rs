use std::{iter::Peekable, str::Chars};
use anyhow::Result;
use thiserror::Error;
use crate::token::Token;

#[derive(Debug, Error)]
pub enum LexErr {
    #[error("Failed to parse '{0}' as i64")]
    NumericParse(String),

    #[error("Invalid escape character '\\{0}'")]
    InvalidStrEscape(char),

    #[error("Lexer encountered unexpected char: '{0}'")]
    UnexpectedChar(char),

    #[error("Missing closing string quote '\"'")]
    MissingClosingString
}

struct Lexer<'a> {
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
                '&' if matches!(self.peek(), '&') => {
                    self.next();
                    tokens.push(Token::LAnd);
                }
                '|' if matches!(self.peek(), '|') => {
                    self.next();
                    tokens.push(Token::LOr);
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
                '{' => tokens.push(Token::LBraces),
                '}' => tokens.push(Token::RBraces),
                '!' => tokens.push(Token::LNot),

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
                    let i64val = sval.parse().map_err(|_| LexErr::NumericParse(sval))?;
                    tokens.push(Token::I64(i64val));
                }

                // Lex a string
                '"' => {
                    // Opening " was consumed in match

                    let mut text = String::new();

                    loop {
                        match self.peek() {
                            '"' => break,
                            '\0' => Err(LexErr::MissingClosingString)?,
                            _ => {

                                match self.next() {
                                    '\\' => {
                                        match self.next() {
                                            'n' => text.push('\n'),
                                            'r' => text.push('\r'),
                                            't' => text.push('\t'),
                                            '\\' => text.push('\\'),
                                            '"' => text.push('"'),
                                            ch => Err(LexErr::InvalidStrEscape(ch))?,
                                        }
                                    }
                                    ch => text.push(ch),
                                }
                            }
                        }
                    }

                    // Consume closing "
                    self.next();

                    tokens.push(Token::String(text))
                    
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

                    let token = match ident.as_str() {
                        "loop" => Token::Loop,
                        "print" => Token::Print,
                        "if" => Token::If,
                        "else" => Token::Else,
                        _ => Token::Ident(ident),
                    };

                    tokens.push(token);
                }

                //TODO: Don't panic, keep calm
                ch => Err(LexErr::UnexpectedChar(ch))?,
            }
        }

        Ok(tokens)
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
pub fn lex(code: &str) -> Result<Vec<Token>, LexErr> {
    let mut lexer = Lexer::new(code);
    lexer.lex()
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
