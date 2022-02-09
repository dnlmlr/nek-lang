use std::iter::Peekable;

use crate::{
    ast::{Ast, BinOpType, BlockScope, Expression, If, Loop, Statement, UnOpType},
    stringstore::{Sid, StringStore},
    token::Token,
    T,
};

/// Parse the given tokens into an abstract syntax tree
pub fn parse<T: Iterator<Item = Token>, A: IntoIterator<IntoIter = T>>(tokens: A) -> Ast {
    let parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    string_store: StringStore,
    var_stack: Vec<Sid>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Create a new parser to parse the given Token Stream
    pub fn new<A: IntoIterator<IntoIter = T>>(tokens: A) -> Self {
        let tokens = tokens.into_iter().peekable();
        let string_store = StringStore::new();
        let var_stack = Vec::new();
        Self {
            tokens,
            string_store,
            var_stack,
        }
    }

    pub fn parse(mut self) -> Ast {
        let main = self.parse_scoped_block();
        Ast {
            main,
            stringstore: self.string_store,
        }
    }

    /// Parse tokens into an abstract syntax tree. This will continuously parse statements until
    /// encountering end-of-file or a block end '}' .
    fn parse_scoped_block(&mut self) -> BlockScope {
        let framepointer = self.var_stack.len();
        let mut prog = Vec::new();

        loop {
            match self.peek() {
                T![;] => {
                    self.next();
                }
                T![EoF] | T!['}'] => break,

                T!['{'] => {
                    self.next();
                    prog.push(Statement::Block(self.parse_scoped_block()));
                    if self.next() != T!['}'] {
                        panic!("Error parsing block: Expectected closing braces '}}'");
                    }
                }

                // By default try to lex a statement
                _ => prog.push(self.parse_stmt()),
            }
        }

        self.var_stack.truncate(framepointer);

        prog
    }

    /// Parse a single statement from the tokens.
    fn parse_stmt(&mut self) -> Statement {
        match self.peek() {
            T![loop] => Statement::Loop(self.parse_loop()),

            T![print] => {
                self.next();

                let expr = self.parse_expr();

                // After a statement, there must be a semicolon
                if self.next() != T![;] {
                    panic!("Expected semicolon after statement");
                }

                Statement::Print(expr)
            }

            T![if] => Statement::If(self.parse_if()),

            // If it is not a loop, try to lex as an expression
            _ => {
                let mut expr = self.parse_expr();

                match &mut expr {
                    Expression::BinOp(BinOpType::Declare, lhs, _) => match lhs.as_mut() {
                        Expression::Var(sid, sp) => {
                            *sp = self.var_stack.len();
                            self.var_stack.push(*sid);
                        }
                        _ => panic!("Left hand side of declaration must be variable"),
                    },
                    _ => (),
                }

                let stmt = Statement::Expr(expr);

                // After a statement, there must be a semicolon
                if self.next() != T![;] {
                    panic!("Expected semicolon after statement");
                }

                stmt
            }
        }
    }

    /// Parse an if statement from the tokens
    fn parse_if(&mut self) -> If {
        if self.next() != T![if] {
            panic!("Error lexing if: Expected if token");
        }

        let condition = self.parse_expr();

        if self.next() != T!['{'] {
            panic!("Error lexing if: Expected '{{'")
        }

        let body_true = self.parse_scoped_block();

        if self.next() != T!['}'] {
            panic!("Error lexing if: Expected '}}'")
        }

        let mut body_false = BlockScope::default();

        if self.peek() == &T![else] {
            self.next();

            if self.next() != T!['{'] {
                panic!("Error lexing if: Expected '{{'")
            }

            body_false = self.parse_scoped_block();

            if self.next() != T!['}'] {
                panic!("Error lexing if: Expected '}}'")
            }
        }

        If {
            condition,
            body_true,
            body_false,
        }
    }

    /// Parse a loop statement from the tokens
    fn parse_loop(&mut self) -> Loop {
        if self.next() != T![loop] {
            panic!("Error lexing loop: Expected loop token");
        }

        let condition = self.parse_expr();
        let mut advancement = None;

        let body;

        match self.next() {
            T!['{'] => {
                body = self.parse_scoped_block();
            }

            T![;] => {
                advancement = Some(self.parse_expr());

                if self.next() != T!['{'] {
                    panic!("Error lexing loop: Expected '{{'")
                }

                body = self.parse_scoped_block();
            }

            _ => panic!("Error lexing loop: Expected ';' or '{{'"),
        }

        if self.next() != T!['}'] {
            panic!("Error lexing loop: Expected '}}'")
        }

        Loop {
            condition,
            advancement,
            body,
        }
    }

    /// Parse a single expression from the tokens
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
            T![i64(val)] => Expression::I64(val),

            // Literal String
            T![str(text)] => Expression::String(self.string_store.intern_or_lookup(&text)),

            // Array literal. Square brackets containing the array size as expression
            T!['['] => {
                let size = self.parse_expr();

                if self.next() != T![']'] {
                    panic!("Error parsing array literal: Expected closing bracket")
                }

                Expression::ArrayLiteral(size.into())
            }

            // Array sccess, aka indexing. An ident followed by square brackets containing the
            // index as an expression
            T![ident(name)] if self.peek() == &T!['['] => {
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid);

                self.next();

                let index = self.parse_expr();

                if self.next() != T![']'] {
                    panic!("Error parsing array access: Expected closing bracket")
                }

                Expression::ArrayAccess(sid, stackpos, index.into())
            }

            T![ident(name)] => {
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid);
                Expression::Var(sid, stackpos)
            }

            // Parentheses grouping
            T!['('] => {
                let inner_expr = self.parse_expr();

                // Verify that there is a closing parenthesis
                if self.next() != T![')'] {
                    panic!("Error parsing primary expr: Exepected closing parenthesis ')'");
                }

                inner_expr
            }

            // Unary negation
            T![-] => {
                let operand = self.parse_primary();
                Expression::UnOp(UnOpType::Negate, operand.into())
            }

            // Unary bitwise not (bitflip)
            T![~] => {
                let operand = self.parse_primary();
                Expression::UnOp(UnOpType::BNot, operand.into())
            }

            // Unary logical not
            T![!] => {
                let operand = self.parse_primary();
                Expression::UnOp(UnOpType::LNot, operand.into())
            }

            tok => panic!("Error parsing primary expr: Unexpected Token '{:?}'", tok),
        }
    }

    fn get_stackpos(&self, varid: Sid) -> usize {
        self.var_stack
            .iter()
            .rev()
            .position(|it| *it == varid)
            .map(|it| self.var_stack.len() - it - 1)
            .unwrap_or(usize::MAX)
    }

    /// Get the next Token without removing it
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&T![EoF])
    }

    /// Advance to next Token and return the removed Token
    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(T![EoF])
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BinOpType, Expression, Statement},
        parser::parse,
        T,
    };

    #[test]
    fn test_parser() {
        // Expression: 1 + 2 * 3 - 4
        // With precedence: (1 + (2 * 3)) - 4
        let tokens = [
            T![i64(1)],
            T![+],
            T![i64(2)],
            T![*],
            T![i64(3)],
            T![-],
            T![i64(4)],
            T![;],
        ];

        let expected = Statement::Expr(Expression::BinOp(
            BinOpType::Sub,
            Expression::BinOp(
                BinOpType::Add,
                Expression::I64(1).into(),
                Expression::BinOp(
                    BinOpType::Mul,
                    Expression::I64(2).into(),
                    Expression::I64(3).into(),
                )
                .into(),
            )
            .into(),
            Expression::I64(4).into(),
        ));

        let expected = vec![expected];

        let actual = parse(tokens);
        assert_eq!(expected, actual.main);
    }
}
