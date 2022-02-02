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
pub enum Statement {
    Expr(Expression),
    Loop(Loop),
    Print(Expression),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub prog: Vec<Statement>
}

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
        let mut prog = Vec::new();

        loop {
            match self.peek() {
                Token::Semicolon => {
                    self.next();
                }
                Token::EoF => break,
                Token::RBraces => {
                    self.next();
                    break;
                }

                // By default try to lex a statement
                _ => {
                    prog.push(self.parse_stmt())
                }
            }
        }

        Ast { prog }

    }

    fn parse_stmt(&mut self) -> Statement {
        match self.peek() {
            Token::Loop => Statement::Loop(self.parse_loop()),

            Token::Print => {
                self.next();

                let expr = self.parse_expr();

                // After a statement, there must be a semicolon
                if !matches!(self.next(), Token::Semicolon) {
                    panic!("Expected semicolon after statement");
                }

                Statement::Print(expr)
            }

            // If it is not a loop, try to lex as an expression
            _ => {
                let stmt = Statement::Expr(self.parse_expr());

                // After a statement, there must be a semicolon
                if !matches!(self.next(), Token::Semicolon) {
                    panic!("Expected semicolon after statement");
                }

                stmt
            }
        }
    }

    // loop i < 1_000; i = i +1 {
    //     if i % 3 == 0 | i % 5 == 0 {
    //         sum = sum + i;
    //     }
    // }

    fn parse_loop(&mut self) -> Loop {
        if !matches!(self.next(), Token::Loop) {
            panic!("Error lexing loop: Expected loop token");
        }

        let condition = self.parse_expr();
        let mut advancement = None;

        let body;

        match self.next() {
            Token::LBraces => {
                body = self.parse();
            }

            Token::Semicolon => {
                advancement = Some(self.parse_expr());

                if !matches!(self.next(), Token::LBraces) {
                    panic!("Error lexing loop: Expected '{{'")
                }

                body = self.parse();
            },

            _ => panic!("Error lexing loop: Expected ';' or '{{'")
        }

        Loop { condition, advancement, body }

    }

    fn parse_expr(&mut self) -> Expression {
        let lhs = self.parse_primary();
        self.parse_expr_precedence(lhs, 0)
    }

    /// Parse binary expressions with a precedence equal to or higher than min_prec
    fn parse_expr_precedence(&mut self, mut lhs: Expression, min_prec: u8) -> Expression {
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

            lhs = Expression::BinOp(binop, lhs.into(), rhs.into());
        }

        lhs
    }

    /// Parse a primary expression (for now only number)
    fn parse_primary(&mut self) -> Expression {
        match self.next() {
            // Literal i64
            Token::I64(val) => Expression::I64(val),

            Token::Ident(name) => Expression::Var(name),

            // Parentheses grouping
            Token::LParen => {
                let inner_expr = self.parse_expr();

                // Verify that there is a closing parenthesis
                if !matches!(self.next(), Token::RParen) {
                    panic!("Error parsing primary expr: Exepected closing parenthesis ')'");
                }

                inner_expr
            }

            // Unary negation
            Token::Sub => {
                let operand = self.parse_primary();
                Expression::UnOp(UnOpType::Negate, operand.into())
            }

            Token::Tilde => {
                let operand = self.parse_primary();
                Expression::UnOp(UnOpType::BNot, operand.into())
            }

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
    ///
    /// The operator precedences are derived from the C language operator precedences. While not all
    /// C operators are included or the exact same, the precedence oder is the same.
    /// See: https://en.cppreference.com/w/c/language/operator_precedence

    fn precedence(&self) -> u8 {
        match self {
            BinOpType::Declare => 0,
            BinOpType::Assign => 1,
            BinOpType::BOr => 2,
            BinOpType::BXor => 3,
            BinOpType::BAnd => 4,
            BinOpType::EquEqu | BinOpType::NotEqu => 5,
            BinOpType::Less | BinOpType::LessEqu | BinOpType::Greater | BinOpType::GreaterEqu => 6,
            BinOpType::Shl | BinOpType::Shr => 7,
            BinOpType::Add | BinOpType::Sub => 8,
            BinOpType::Mul | BinOpType::Div | BinOpType::Mod => 9,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, Expression, BinOpType};
    use crate::{lexer::Token, parser::{Statement, Ast}};

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

        let expected = Statement::Expr(Expression::BinOp(
            BinOpType::Sub,
            Expression::BinOp(
                BinOpType::Add,
                Expression::I64(1).into(),
                Expression::BinOp(BinOpType::Mul, Expression::I64(2).into(), Expression::I64(3).into()).into(),
            )
            .into(),
            Expression::I64(4).into(),
        ));

        let expected = Ast { prog: vec![expected] };

        let actual = parse(tokens);
        assert_eq!(expected, actual);
    }
}
