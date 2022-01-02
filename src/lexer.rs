use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub enum Token {
    I64(i64),

    Add,

    Mul,
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
                ' ' => (),
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

    /// Shows next character
    fn peek(&mut self) -> Option<char> {
        self.code.peek().copied()
    }
}

pub fn lex(code: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(code);
    lexer.lex()
}
