use thiserror::Error;

use crate::{
    ast::{Ast, BlockScope, Expression, FunDecl, If, Loop, Statement, VarDecl},
    stringstore::{Sid, StringStore},
    token::Token,
    util::{PutBackIter, PutBackableExt},
    T,
};

/// Errors that can occur while parsing
#[derive(Debug, Error)]
pub enum ParseErr {
    #[error("Unexpected Token \"{0:?}\", expected \"{1}\"")]
    UnexpectedToken(Token, String),

    #[error("Left hand side of declaration is not a variable")]
    DeclarationOfNonVar,

    #[error("Use of undefined variable \"{0}\"")]
    UseOfUndeclaredVar(String),

    #[error("Use of undefined function \"{0}\"")]
    UseOfUndeclaredFun(String),

    #[error("Redeclation of function \"{0}\"")]
    RedeclarationFun(String),

    #[error("Function not declared at top level \"{0}\"")]
    FunctionOnNonTopLevel(String),
}

/// A result that can either be Ok, or a ParseErr
type ResPE<T> = Result<T, ParseErr>;

/// This macro can be used to quickly and easily assert if the next token is matching the expected
/// token and return an appropriate error if not. Since this is intended to be used inside the 
/// parser, the first argument should always be `self`.
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

/// A parser that takes in a Token Stream and can create a full abstract syntax tree from it.
struct Parser<T: Iterator<Item = Token>> {
    tokens: PutBackIter<T>,
    string_store: StringStore,
    var_stack: Vec<Sid>,
    fun_stack: Vec<Sid>,
    nesting_level: usize,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Create a new parser to parse the given Token Stream
    pub fn new<A: IntoIterator<IntoIter = T>>(tokens: A) -> Self {
        let tokens = tokens.into_iter().putbackable();
        let string_store = StringStore::new();
        let var_stack = Vec::new();
        let fun_stack = Vec::new();
        Self {
            tokens,
            string_store,
            var_stack,
            fun_stack,
            nesting_level: 0,
        }
    }

    /// Consume the parser and try to create the abstract syntax tree from the token stream
    pub fn parse(mut self) -> ResPE<Ast> {
        let main = self.parse_scoped_block()?;
        Ok(Ast {
            main,
            stringstore: self.string_store,
        })
    }

    /// Parse a series of statements together as a BlockScope. This will continuously parse 
    /// statements until encountering end-of-file or a block end '}' .
    fn parse_scoped_block(&mut self) -> ResPE<BlockScope> {
        self.parse_scoped_block_fp_offset(0)
    }

    /// Same as parse_scoped_block, but an offset to the framepointer can be specified to allow
    /// for easily passing variables into scopes from the outside. This is used when parsing 
    /// function calls
    fn parse_scoped_block_fp_offset(&mut self, framepointer_offset: usize) -> ResPE<BlockScope> {
        self.nesting_level += 1;
        let framepointer = self.var_stack.len() - framepointer_offset;
        let mut prog = Vec::new();

        loop {
            match self.peek() {
                // Just a semicolon is an empty statement. So just consume it
                T![;] => {
                    self.next();
                }

                // '}' end the current block and EoF ends everything, as the end of the tokenstream 
                // is reached
                T![EoF] | T!['}'] => break,

                // Create a new scoped block
                T!['{'] => {
                    self.next();
                    prog.push(Statement::Block(self.parse_scoped_block()?));

                    validate_next!(self, T!['}'], "}");
                }

                // By default try to lex statements
                _ => prog.push(self.parse_stmt()?),
            }
        }

        // Reset the stack to where it was before entering the scope
        self.var_stack.truncate(framepointer);
        self.nesting_level -= 1;

        Ok(prog)
    }

    /// Parse a single statement from the tokens
    fn parse_stmt(&mut self) -> ResPE<Statement> {
        let stmt = match self.peek() {
            // Break statement
            T![break] => {
                self.next();

                // After the statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                Statement::Break
            }

            // Continue statement
            T![continue] => {
                self.next();

                // After the statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                Statement::Continue
            }

            // Loop statement
            T![loop] => Statement::Loop(self.parse_loop()?),

            // Print statement
            T![print] => {
                self.next();

                let expr = self.parse_expr()?;

                // After the statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                Statement::Print(expr)
            }

            // Return statement
            T![return] => {
                self.next();
                let stmt = Statement::Return(self.parse_expr()?);

                // After a statement, there must be a semicolon
                validate_next!(self, T![;], ";");

                stmt
            }

            // If statement
            T![if] => Statement::If(self.parse_if()?),

            // Function definition statement
            T![fun] => {

                self.next();

                // Expect an identifier as the function name
                let fun_name = match self.next() {
                    T![ident(fun_name)] => fun_name,
                    tok => return Err(ParseErr::UnexpectedToken(tok, "<ident>".to_string())),
                };
                
                // Only allow function definitions on the top level
                if self.nesting_level > 1 {
                    return Err(ParseErr::FunctionOnNonTopLevel(fun_name));
                }

                // Intern the function name
                let fun_name = self.string_store.intern_or_lookup(&fun_name);

                // Check if the function name already exists
                if self.fun_stack.contains(&fun_name) {
                    return Err(ParseErr::RedeclarationFun(
                        self.string_store
                            .lookup(fun_name)
                            .cloned()
                            .unwrap_or("<unknown>".to_string()),
                    ));
                }

                // Put the function name on the fucntion stack for precalculating the stack 
                // positions
                let fun_stackpos = self.fun_stack.len();
                self.fun_stack.push(fun_name);


                let mut arg_names = Vec::new();

                validate_next!(self, T!['('], "(");

                // Parse the optional arguments inside the parentheses
                while matches!(self.peek(), T![ident(_)]) {
                    let var_name = match self.next() {
                        T![ident(var_name)] => var_name,
                        _ => unreachable!(),
                    };

                    // Intern argument names 
                    let var_name = self.string_store.intern_or_lookup(&var_name);
                    arg_names.push(var_name);

                    // Push the variable onto the varstack
                    self.var_stack.push(var_name);

                    // If there are more args skip the comma so that the loop will read the argname
                    if self.peek() == &T![,] {
                        self.next();
                    }
                }

                validate_next!(self, T![')'], ")");

                validate_next!(self, T!['{'], "{");

                // Create the scoped block with a stack offset. This will pop the args that are
                // added to the stack while parsing args
                let body = self.parse_scoped_block_fp_offset(arg_names.len())?;

                validate_next!(self, T!['}'], "}");

                Statement::FunDeclare(FunDecl {
                    name: fun_name,
                    fun_stackpos,
                    argnames: arg_names,
                    body: body.into(),
                })
            }

            // Either a variable declaration statement or an expression statement
            _ => {
                // To decide if it is a declaration or an expression, a lookahead is needed 
                let first = self.next();

                let stmt = match (first, self.peek()) {
                    // Identifier and "<-" is a declaration
                    (T![ident(name)], T![<-]) => {
                        self.next();

                        let rhs = self.parse_expr()?;

                        let sid = self.string_store.intern_or_lookup(&name);
                        let sp = self.var_stack.len();
                        self.var_stack.push(sid);

                        Statement::Declaration(VarDecl {
                            name: sid,
                            var_stackpos: sp,
                            rhs,
                        })
                    }
                    // Anything else must be an expression
                    (first, _) => {
                        // Put the first token back in order for the parse_expr to see it 
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

        // Optionally parse the else part
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

        let mut condition = None;
        let mut advancement = None;

        // Check if the optional condition is present
        if !matches!(self.peek(), T!['{']) {
            condition = Some(self.parse_expr()?);

            // Check if the optional advancement is present
            if matches!(self.peek(), T![;]) {
                self.next();
                advancement = Some(self.parse_expr()?);
            }
        }

        validate_next!(self, T!['{'], "{");

        let body = self.parse_scoped_block()?;

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

    /// Parse binary expressions with a precedence equal to or higher than min_prec.
    /// This uses the precedence climbing methode for dealing with the operator precedences:
    /// https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
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

    /// Parse a primary expression. A primary can be a literal value, variable, function call,
    /// array indexing, parentheses grouping or a unary operation
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
                // Get the stack position of the array variable
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid)?;

                self.next();

                let index = self.parse_expr()?;

                validate_next!(self, T![']'], "]");

                Expression::ArrayAccess(sid, stackpos, index.into())
            }

            // Identifier followed by parenthesis is a function call
            T![ident(name)] if self.peek() == &T!['('] => {
                // Skip the opening parenthesis
                self.next();

                let sid = self.string_store.intern_or_lookup(&name);

                let mut args = Vec::new();

                // Parse the arguments as expressions
                while !matches!(self.peek(), T![')']) {
                    let arg = self.parse_expr()?;
                    args.push(arg);

                    // If there are more args skip the comma so that the loop will read the argname
                    if self.peek() == &T![,] {
                        self.next();
                    }
                }

                validate_next!(self, T![')'], ")");

                // Find the function stack position
                let fun_stackpos = self.get_fun_stackpos(sid)?;

                Expression::FunCall(sid, fun_stackpos, args)
            }

            // Just an identifier is a variable
            T![ident(name)] => {
                // Find the variable stack position
                let sid = self.string_store.intern_or_lookup(&name);
                let stackpos = self.get_stackpos(sid)?;

                Expression::Var(sid, stackpos)
            }

            // Parentheses grouping
            T!['('] => {
                // Contained inbetween the parentheses can be any other expression
                let inner_expr = self.parse_expr()?;

                // Verify that there is a closing parenthesis
                validate_next!(self, T![')'], ")");

                inner_expr
            }

            // Unary operations or invalid token
            tok => match tok.try_to_unop() {
                // If the token is a valid unary operation, parse it as such
                Some(uot) => Expression::UnOp(uot, self.parse_primary()?.into()),
                
                // Otherwise it's an unexpected token
                None => return Err(ParseErr::UnexpectedToken(tok, "primary".to_string())),
            },
        };

        Ok(primary)
    }

    /// Try to get the position of a variable on the variable stack. This is needed to precalculate 
    /// the stackpositions in order to save time when executing
    fn get_stackpos(&self, varid: Sid) -> ResPE<usize> {
        self.var_stack
            .iter()
            .rev()
            .position(|it| *it == varid)
            .map(|it| it)
            .ok_or(ParseErr::UseOfUndeclaredVar(
                self.string_store
                    .lookup(varid)
                    .map(String::from)
                    .unwrap_or("<unknown>".to_string()),
            ))
    }

    /// Try to get the position of a function on the function stack. This is needed to precalculate 
    /// the stackpositions in order to save time when executing
    fn get_fun_stackpos(&self, varid: Sid) -> ResPE<usize> {
        self.fun_stack
            .iter()
            .rev()
            .position(|it| *it == varid)
            .map(|it| self.fun_stack.len() - it - 1)
            .ok_or(ParseErr::UseOfUndeclaredFun(
                self.string_store
                    .lookup(varid)
                    .map(String::from)
                    .unwrap_or("<unknown>".to_string()),
            ))
    }

    /// Get the next Token without removing it. If there are no more tokens left, the EoF token is 
    /// returned. This follows the same reasoning as in the Lexer
    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap_or(&T![EoF])
    }

    /// Put a single token back into the token stream
    fn putback(&mut self, tok: Token) {
        self.tokens.putback(tok);
    }

    /// Advance to next Token and return the removed Token. If there are no more tokens left, the 
    /// EoF token is returned. This follows the same reasoning as in the Lexer
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

    /// A very simple test to check if the parser correctly parses a simple expression
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
