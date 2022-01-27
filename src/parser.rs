use std::iter::Peekable;

use crate::lexer::Token;

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

    /// Bitwise OR (inclusive or)
    BOr,

    /// Bitwise And
    BAnd,

    /// Bitwise Xor (exclusive or)
    BXor,

    /// Shift Left
    Shl,

    /// Shift Right
    Shr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ast {
    /// Integer literal (64-bit)
    I64(i64),
    /// Binary operation. Consists of type, left hand side and right hand side
    BinOp(BinOpType, Box<Ast>, Box<Ast>),
}

/*
## Grammar
### Expressions
`expr_primary = LITERAL` \
`expr_mul = expr_primary (("*" | "/") expr_primary)*` \
`expr_add = expr_mul (("+" | "-") expr_mul)*` \
`expr = expr_add` \
*/

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Create a new parser to parse the given Token Stream
    fn new<A: IntoIterator<IntoIter = T>>(tokens: A) -> Self {
        let tokens = tokens.into_iter().peekable();
        Self { tokens }
    }

    fn parse(&mut self) -> Ast {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Ast {
        let lhs = self.parse_primary();
        self.parse_expr_precedence(lhs, 0)
    }

    /// Parse binary expressions with a precedence equal to or higher than min_prec
    fn parse_expr_precedence(&mut self, mut lhs: Ast, min_prec: u8) -> Ast {
        while let Some(binop) = &self.peek().try_to_binop() {
            // Stop if the next operator has a lower binding power
            if !(binop.precedence() >= min_prec) {
                break;
            }

            // The while condition already verified that this is some while peeking, so unwrap is
            // valid
            let binop = self.next().try_to_binop().unwrap();

            let mut rhs = self.parse_primary();

            while let Some(binop2) = &self.peek().try_to_binop() {
                if !(binop2.precedence() > binop.precedence()) {
                    break;
                }

                rhs = self.parse_expr_precedence(rhs, binop.precedence() + 1);
            }

            lhs = Ast::BinOp(binop, lhs.into(), rhs.into());
        }

        lhs
    }

    /// Parse a primary expression (for now only number)
    fn parse_primary(&mut self) -> Ast {
        match self.next() {
            Token::I64(val) => Ast::I64(val),

            tok => panic!("Error parsing primary expr: Unexpected Token '{:?}'", tok),
        }
    }

    /// Get the next Token without removing it
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&Token::EoF)
    }

    /// Advance to next Token and return the removed Token
    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token::EoF)
    }
}

pub fn parse<T: Iterator<Item = Token>, A: IntoIterator<IntoIter = T>>(tokens: A) -> Ast {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

impl BinOpType {
    /// Get the precedence for a binary operator. Higher value means the OP is stronger binding.
    /// For example Multiplication is stronger than addition, so Mul has higher precedence than Add.
    fn precedence(&self) -> u8 {
        match self {
            BinOpType::BOr => 0,
            BinOpType::BXor => 1,
            BinOpType::BAnd => 2,
            BinOpType::Shl | BinOpType::Shr => 3,
            BinOpType::Add | BinOpType::Sub => 4,
            BinOpType::Mul | BinOpType::Div | BinOpType::Mod => 5,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, Ast, BinOpType};
    use crate::lexer::Token;

    #[test]
    fn test_parser() {
        // Expression: 1 + 2 * 3 + 4
        // With precedence: (1 + (2 * 3)) + 4
        let tokens = [
            Token::I64(1),
            Token::Add,
            Token::I64(2),
            Token::Mul,
            Token::I64(3),
            Token::Sub,
            Token::I64(4),
        ];

        let expected = Ast::BinOp(
            BinOpType::Sub,
            Ast::BinOp(
                BinOpType::Add,
                Ast::I64(1).into(),
                Ast::BinOp(BinOpType::Mul, Ast::I64(2).into(), Ast::I64(3).into()).into(),
            )
            .into(),
            Ast::I64(4).into(),
        );

        let actual = parse(tokens);
        assert_eq!(expected, actual);
    }
}
