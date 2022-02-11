use std::{iter::Peekable, str::Chars};
use thiserror::Error;

use crate::{token::Token, T};

/// Errors that can occur while lexing a given string
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
    let lexer = Lexer::new(code);
    lexer.lex()
}

/// The lexer is created from a reference to a sourcecode string and is consumed to create a token 
/// buffer from that sourcecode.
struct Lexer<'a> {
    /// The sourcecode text as a peekable iterator over the chars. Peekable allows for look-ahead 
    /// and the use of the Chars iterator allows to support unicode characters
    code: Peekable<Chars<'a>>,
    /// The lexed tokens
    tokens: Vec<Token>,
    /// The sourcecode character that is currently being lexed
    current_char: char,
}

impl<'a> Lexer<'a> {

    /// Create a new lexer from the given sourcecode
    fn new(code: &'a str) -> Self {
        let code = code.chars().peekable();
        let tokens = Vec::new();
        let current_char = '\0';
        Self {
            code,
            tokens,
            current_char,
        }
    }

    /// Consume the lexer and try to lex the contained sourcecode into a token buffer
    fn lex(mut self) -> Result<Vec<Token>, LexErr> {

        loop {
            self.current_char = self.next();
            // Match on the current and next character. This gives a 1-char look-ahead and
            // can be used to directly match 2-char tokens
            match (self.current_char, self.peek()) {
                // Stop lexing at EOF
                ('\0', _) => break,

                // Skip / ignore whitespace
                (' ' | '\t' | '\n' | '\r', _) => (),

                // Line comment. Consume every char until linefeed (next line)
                ('/', '/') => while !matches!(self.next(), '\n' | '\0') {},

                // Double character tokens
                ('>', '>') => self.push_tok_consume(T![>>]),
                ('<', '<') => self.push_tok_consume(T![<<]),
                ('=', '=') => self.push_tok_consume(T![==]),
                ('!', '=') => self.push_tok_consume(T![!=]),
                ('<', '=') => self.push_tok_consume(T![<=]),
                ('>', '=') => self.push_tok_consume(T![>=]),
                ('<', '-') => self.push_tok_consume(T![<-]),
                ('&', '&') => self.push_tok_consume(T![&&]),
                ('|', '|') => self.push_tok_consume(T![||]),

                // Single character tokens
                (',', _) => self.push_tok(T![,]),
                (';', _) => self.push_tok(T![;]),
                ('+', _) => self.push_tok(T![+]),
                ('-', _) => self.push_tok(T![-]),
                ('*', _) => self.push_tok(T![*]),
                ('/', _) => self.push_tok(T![/]),
                ('%', _) => self.push_tok(T![%]),
                ('|', _) => self.push_tok(T![|]),
                ('&', _) => self.push_tok(T![&]),
                ('^', _) => self.push_tok(T![^]),
                ('(', _) => self.push_tok(T!['(']),
                (')', _) => self.push_tok(T![')']),
                ('~', _) => self.push_tok(T![~]),
                ('<', _) => self.push_tok(T![<]),
                ('>', _) => self.push_tok(T![>]),
                ('=', _) => self.push_tok(T![=]),
                ('{', _) => self.push_tok(T!['{']),
                ('}', _) => self.push_tok(T!['}']),
                ('!', _) => self.push_tok(T![!]),
                ('[', _) => self.push_tok(T!['[']),
                (']', _) => self.push_tok(T![']']),

                // Special tokens with variable length

                // Lex multiple characters together as numbers
                ('0'..='9', _) => self.lex_number()?,

                // Lex multiple characters together as a string
                ('"', _) => self.lex_str()?,

                // Lex multiple characters together as identifier or keyword
                ('a'..='z' | 'A'..='Z' | '_', _) => self.lex_identifier()?,

                // Any character that was not handled otherwise is invalid
                (ch, _) => Err(LexErr::UnexpectedChar(ch))?,
            }
        }

        Ok(self.tokens)
    }

    /// Lex multiple characters as a number until encountering a non numeric digit. The
    /// successfully lexed i64 literal token is appended to the stored tokens.
    fn lex_number(&mut self) -> Result<(), LexErr> {
        // String representation of the integer value
        let mut sval = String::from(self.current_char);

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

        // Try to convert the string representation of the value to i64. The error is mapped to
        // the appropriate LexErr
        let i64val = sval.parse().map_err(|_| LexErr::NumericParse(sval))?;

        self.push_tok(T![i64(i64val)]);

        Ok(())
    }

    /// Lex characters as a string until encountering an unescaped closing doublequoute char '"'.
    /// The successfully lexed string literal token is appended to the stored tokens.
    fn lex_str(&mut self) -> Result<(), LexErr> {
        // The opening " was consumed in match, so a fresh string can be used
        let mut text = String::new();

        // Read all chars until encountering the closing "
        loop {
            match self.peek() {
                // An unescaped doubleqoute ends the current string
                '"' => break,

                // If the end of file is reached while still waiting for '"', error out
                '\0' => Err(LexErr::MissingClosingString)?,

                _ => match self.next() {
                    // Backslash indicates an escaped character, so consume one more char and
                    // treat it as the escaped char
                    '\\' => match self.next() {
                        'n' => text.push('\n'),
                        'r' => text.push('\r'),
                        't' => text.push('\t'),
                        '\\' => text.push('\\'),
                        '"' => text.push('"'),
                        // If the escaped char is not handled, it is unsupported and an error
                        ch => Err(LexErr::InvalidStrEscape(ch))?,
                    },
                    // All other characters are simply appended to the string
                    ch => text.push(ch),
                },
            }
        }

        // Consume closing "
        self.next();

        self.push_tok(T![str(text)]);

        Ok(())
    }

    /// Lex characters from the text as an identifier. The successfully lexed ident or keyword
    /// token is appended to the stored tokens.
    fn lex_identifier(&mut self) -> Result<(), LexErr> {
        let mut ident = String::from(self.current_char);

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
            "loop" => T![loop],
            "print" => T![print],
            "if" => T![if],
            "else" => T![else],
            "fun" => T![fun],
            "return" => T![return],
            "break" => T![break],
            "continue" => T![continue],

            // If it doesn't match a keyword, it is a normal identifier
            _ => T![ident(ident)],
        };

        self.push_tok(token);

        Ok(())
    }

    /// Push the given token into the stored tokens
    fn push_tok(&mut self, token: Token) {
        self.tokens.push(token);
    }

    /// Same as `push_tok` but also consumes the next token, removing it from the code iter. This
    /// is useful when lexing double char tokens where the second token has only been peeked.
    fn push_tok_consume(&mut self, token: Token) {
        self.next();
        self.tokens.push(token);
    }

    /// Advance to next character and return the removed char. When the end of the code is reached,
    /// `'\0'` is returned. This is used instead of an Option::None since it allows for much 
    /// shorter and cleaner code in the main loop. The `'\0'` character would not be valid anyways
    fn next(&mut self) -> char {
        self.code.next().unwrap_or('\0')
    }

    /// Get the next character without removing it. When the end of the code is reached,
    /// `'\0'` is returned. This is used instead of an Option::None since it allows for much 
    /// shorter and cleaner code in the main loop. The `'\0'` character would not be valid anyways
    fn peek(&mut self) -> char {
        self.code.peek().copied().unwrap_or('\0')
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, T};

    /// A general test to check if the lexer actually lexes tokens correctly
    #[test]
    fn test_lexer() {
        let code = r#"53+1-567_000 * / % | ~ ! < > & ^ ({[]});= <- >= <=
            == != && || << >> loop if else print my_123var "hello \t world\r\n\"\\""#;
        let expected = vec![
            T![i64(53)],
            T![+],
            T![i64(1)],
            T![-],
            T![i64(567_000)],
            T![*],
            T![/],
            T![%],
            T![|],
            T![~],
            T![!],
            T![<],
            T![>],
            T![&],
            T![^],
            T!['('],
            T!['{'],
            T!['['],
            T![']'],
            T!['}'],
            T![')'],
            T![;],
            T![=],
            T![<-],
            T![>=],
            T![<=],
            T![==],
            T![!=],
            T![&&],
            T![||],
            T![<<],
            T![>>],
            T![loop],
            T![if],
            T![else],
            T![print],
            T![ident("my_123var".to_string())],
            T![str("hello \t world\r\n\"\\".to_string())],
        ];

        let actual = lex(code).unwrap();
        assert_eq!(expected, actual);
    }
}
