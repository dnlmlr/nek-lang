use thiserror::Error;

use crate::{
    ast::{Ast, BlockScope, Expression, If, Loop, Statement},
    stringstore::{Sid, StringStore},
    token::Token,
    util::{PutBackIter, PutBackableExt},
    T,
};

#[derive(Debug, Error)]
pub enum ParseErr {
    #[error("Unexpected Token \"{0:?}\", expected \"{1}\"")]
    UnexpectedToken(Token, String),
    #[error("Left hand side of declaration is not a variable")]
    DeclarationOfNonVar,
    #[error("Use of undefined variable \"{0}\"")]
    UseOfUndeclaredVar(String),
}

type ResPE<T> = Result<T, ParseErr>;

macro_rules! validate_next {
    ($self:ident, $expected_tok:pat, $expected_str:expr) => {
        match $self.next() {
            $expected_tok => (),
            tok => return Err(ParseErr::UnexpectedToken(tok, format!("{}", $expected_str))),
        }
    };
}

/// Parse the given tokens into an abstract syntax tree
pub fn parse<T: Iterator<Item = Token>, A: IntoIterator<IntoIter = T>>(tokens: A) -> ResPE<Ast> {
    let parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: PutBackIter<T>,
    string_store: StringStore,
    var_stack: Vec<Sid>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Create a new parser to parse the given Token Stream
    pub fn new<A: IntoIterator<IntoIter = T>>(tokens: A) -> Self {
        let tokens = tokens.into_iter().putbackable();
        let string_store = StringStore::new();
        let var_stack = Vec::new();
        Self {
            tokens,
            string_store,
            var_stack,
        }
    }

    pub fn parse(mut self) -> ResPE<Ast> {
        let main = self.parse_scoped_block()?;
        Ok(Ast {
            main,
            stringstore: self.string_store,
        })
    }

    /// Parse tokens into an abstract syntax tree. This will continuously parse statements until
    /// encountering end-of-file or a block end '}' .
    fn parse_scoped_block(&mut self) -> ResPE<BlockScope> {
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
                    prog.push(Statement::Block(self.parse_scoped_block()?));

                    validate_next!(self, T!['}'], "}");
                }

                // By default try to lex a statement
                _ => prog.push(self.parse_stmt()?),
            }
        }

        self.var_stack.truncate(framepointer);

        Ok(prog)
    }

    /// Parse a single statement from the tokens.
    fn parse_stmt(&mut self) -> ResPE<Statement> {
        let stmt = match self.peek() {
            T![loop] => Statement::Loop(self.parse_loop()?),

            T![print] => {
                self.next();

                let expr = self.parse_expr()?;

                // After a statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                Statement::Print(expr)
            }

            T![if] => Statement::If(self.parse_if()?),

            _ => {
                let first = self.next();

                let stmt = match (first, self.peek()) {
                    (T![ident(name)], T![<-]) => {
                        self.next();

                        let sid = self.string_store.intern_or_lookup(&name);
                        let sp = self.var_stack.len();
                        self.var_stack.push(sid);

                        let rhs = self.parse_expr()?;
                        Statement::Declaration(sid, sp, rhs)
                    }
                    (first, _) => {
                        self.putback(first);
                        Statement::Expr(self.parse_expr()?)
                    }
                };

                // After a statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                stmt
            }
        };
        Ok(stmt)
    }

    /// Parse an if statement from the tokens
    fn parse_if(&mut self) -> ResPE<If> {
        validate_next!(self, T![if], "if");

        let condition = self.parse_expr()?;

        validate_next!(self, T!['{'], "{");

        let body_true = self.parse_scoped_block()?;

        validate_next!(self, T!['}'], "}");

        let mut body_false = BlockScope::default();

        if self.peek() == &T![else] {
            self.next();

            validate_next!(self, T!['{'], "{");

            body_false = self.parse_scoped_block()?;

            validate_next!(self, T!['}'], "}");
        }

        Ok(If {
            condition,
            body_true,
            body_false,
        })
    }

    /// Parse a loop statement from the tokens
    fn parse_loop(&mut self) -> ResPE<Loop> {
        validate_next!(self, T![loop], "loop");

        let condition = self.parse_expr()?;
        let mut advancement = None;

        let body;

        match self.next() {
            T!['{'] => {
                body = self.parse_scoped_block()?;
            }

            T![;] => {
                advancement = Some(self.parse_expr()?);

                validate_next!(self, T!['{'], "{");

                body = self.parse_scoped_block()?;
            }

            tok => return Err(ParseErr::UnexpectedToken(tok, ";\" or \"{".to_string())),
        }

        validate_next!(self, T!['}'], "}");

        Ok(Loop {
            condition,
            advancement,
            body,
        })
    }

    /// Parse a single expression from the tokens
    fn parse_expr(&mut self) -> ResPE<Expression> {
        let lhs = self.parse_primary()?;
        self.parse_expr_precedence(lhs, 0)
    }

    /// Parse binary expressions with a precedence equal to or higher than min_prec
    fn parse_expr_precedence(&mut self, mut lhs: Expression, min_prec: u8) -> ResPE<Expression> {
        while let Some(binop) = &self.peek().try_to_binop() {
            // Stop if the next operator has a lower binding power
            if !(binop.precedence() >= min_prec) {
                break;
            }

            // The while condition already verified that this is some while peeking, so unwrap is
            // valid
            let binop = self.next().try_to_binop().unwrap();

            let mut rhs = self.parse_primary()?;

            while let Some(binop2) = &self.peek().try_to_binop() {
                if !(binop2.precedence() > binop.precedence()) {
                    break;
                }

                rhs = self.parse_expr_precedence(rhs, binop.precedence() + 1)?;
            }

            lhs = Expression::BinOp(binop, lhs.into(), rhs.into());
        }

        Ok(lhs)
    }

    /// Parse a primary expression (for now only number)
    fn parse_primary(&mut self) -> ResPE<Expression> {
        let primary = match self.next() {
            // Literal i64
            T![i64(val)] => Expression::I64(val),

            // Literal String
            T![str(text)] => Expression::String(self.string_store.intern_or_lookup(&text)),

            // Array literal. Square brackets containing the array size as expression
            T!['['] => {
                let size = self.parse_expr()?;

                validate_next!(self, T![']'], "]");

                Expression::ArrayLiteral(size.into())
            }

            // Array sccess, aka indexing. An ident followed by square brackets containing the
            // index as an expression
            T![ident(name)] if self.peek() == &T!['['] => {
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid)?;

                self.next();

                let index = self.parse_expr()?;

                validate_next!(self, T![']'], "]");

                Expression::ArrayAccess(sid, stackpos, index.into())
            }

            T![ident(name)] => {
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid)?;
                Expression::Var(sid, stackpos)
            }

            // Parentheses grouping
            T!['('] => {
                let inner_expr = self.parse_expr()?;

                // Verify that there is a closing parenthesis
                validate_next!(self, T![')'], ")");

                inner_expr
            }

            // Unary operations or invalid token
            tok => match tok.try_to_unop() {
                Some(uot) => Expression::UnOp(uot, self.parse_primary()?.into()),
                None => return Err(ParseErr::UnexpectedToken(tok, "primary".to_string())),
            },
        };

        Ok(primary)
    }

    fn get_stackpos(&self, varid: Sid) -> ResPE<usize> {
        self.var_stack
            .iter()
            .rev()
            .position(|it| *it == varid)
            .map(|it| self.var_stack.len() - it - 1)
            .ok_or(ParseErr::UseOfUndeclaredVar(
                self.string_store
                    .lookup(varid)
                    .map(String::from)
                    .unwrap_or("<unknown>".to_string()),
            ))
    }

    /// Get the next Token without removing it
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&T![EoF])
    }

    fn putback(&mut self, tok: Token) {
        self.tokens.putback(tok);
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

        let actual = parse(tokens).unwrap();
        assert_eq!(expected, actual.main);
    }
}
