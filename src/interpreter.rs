use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

use crate::{
    ast::{Ast, BinOpType, BlockScope, Expression, FunDecl, If, Statement, UnOpType},
    astoptimizer::{AstOptimizer, SimpleAstOptimizer},
    lexer::lex,
    nice_panic,
    parser::parse,
    stringstore::{Sid, StringStore},
};

/// Runtime errors that can occur during execution
#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Invalid array Index: {0:?}")]
    InvalidArrayIndex(Value),

    #[error("Variable used but not declared: {0}")]
    VarUsedNotDeclared(String),

    #[error("Can't index into non-array variable: {0}")]
    TryingToIndexNonArray(String),

    #[error("Invalid value type for unary operation: {0:?}")]
    UnOpInvalidType(Value),

    #[error("Incompatible binary operations. Operands don't match: {0:?} and {1:?}")]
    BinOpIncompatibleTypes(Value, Value),

    #[error("Array access out of bounds: Accessed {0}, size is {1}")]
    ArrayOutOfBounds(usize, usize),

    #[error("Division by zero")]
    DivideByZero,

    #[error("Invalid number of arguments for function {0}. Expected {1}, got {2}")]
    InvalidNumberOfArgs(String, usize, usize),
}

/// Possible variants for the values
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    /// 64-bit integer value
    I64(i64),
    /// String value
    String(Sid),
    /// Array value
    Array(Rc<RefCell<Vec<Value>>>),
    /// Void value
    Void,
}

/// The exit type of a block. When a block ends, the exit type specified why the block ended.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockExit {
    /// Normal exit when the block just ends normally (no returns / breaks / continues / etc.)
    Normal,
    /// The block ended through a break statement. This will be propagated up to the next loop
    /// and cause it to fully terminate
    Break,
    /// The block ended through a continue statement. This will be propagated up to the next loop
    /// and cause it to start the next iteration
    Continue,
    /// The block ended through a return statement. This will propagate up to the next function
    /// body end
    Return(Value),
}

#[derive(Default)]
pub struct Interpreter {
    /// Run the SimpleAstOptimizer over the Ast before executing
    pub optimize_ast: bool,

    /// Print the tokens after lexing
    pub print_tokens: bool,
    /// Print the ast after parsing
    pub print_ast: bool,

    /// Capture the output values of print statements instead of printing them to the terminal
    pub capture_output: bool,
    /// The stored values that were captured
    output: Vec<Value>,

    /// Variable table stores the runtime values of variables as a stack
    vartable: Vec<Value>,

    /// Function table stores the functions during runtime as a stack
    funtable: Vec<FunDecl>,

    /// The stringstore contains all strings used throughout the program
    stringstore: StringStore,
}

impl Interpreter {
    /// Create a new Interpreter
    pub fn new() -> Self {
        Self {
            optimize_ast: true,
            ..Self::default()
        }
    }

    /// Get the captured output
    pub fn output(&self) -> &[Value] {
        &self.output
    }

    /// Try to retrieve a variable value from the varstack. The idx is the index from the back of
    /// the stack. So 0 is the last value, not the first
    fn get_var(&self, idx: usize) -> Option<Value> {
        self.vartable.get(self.vartable.len() - idx - 1).cloned()
    }

    /// Try to retrieve a mutable reference to a variable value from the varstack. The idx is the
    /// index from the back of the stack. So 0 is the last value, not the first
    fn get_var_mut(&mut self, idx: usize) -> Option<&mut Value> {
        let idx = self.vartable.len() - idx - 1;
        self.vartable.get_mut(idx)
    }

    /// Lex, parse and then run the given sourecode. This will terminate the program when an error
    /// occurs and print an appropriate error message.
    pub fn run_str(&mut self, code: &str) {
        // Lex the tokens
        let tokens = match lex(code) {
            Ok(tokens) => tokens,
            Err(e) => nice_panic!("Lexing error: {}", e),
        };

        if self.print_tokens {
            println!("Tokens: {:?}", tokens);
        }

        // Parse the ast
        let ast = match parse(tokens) {
            Ok(ast) => ast,
            Err(e) => nice_panic!("Parsing error: {}", e),
        };

        // Run the ast
        match self.run_ast(ast) {
            Ok(_) => (),
            Err(e) => nice_panic!("Runtime error: {}", e),
        }
    }

    /// Execute the given Ast within the interpreter
    pub fn run_ast(&mut self, mut ast: Ast) -> Result<(), RuntimeError> {
        // Optimize the ast
        if self.optimize_ast {
            ast = SimpleAstOptimizer::optimize(ast);
        }

        if self.print_ast {
            println!("{:#?}", ast.main);
        }

        // Take over the stringstore of the given ast
        self.stringstore = ast.stringstore;

        // Run the top level block (the main)
        self.run_block(&ast.main)?;
        Ok(())
    }

    /// Run all statements in the given block
    pub fn run_block(&mut self, prog: &BlockScope) -> Result<BlockExit, RuntimeError> {
        self.run_block_fp_offset(prog, 0)
    }

    /// Same as run_block, but with an additional framepointer offset. This allows to free more
    /// values from the stack than normally and can be used when passing arguments inside a
    /// function body scope from the outside
    pub fn run_block_fp_offset(
        &mut self,
        prog: &BlockScope,
        framepointer_offset: usize,
    ) -> Result<BlockExit, RuntimeError> {
        let framepointer = self.vartable.len() - framepointer_offset;

        let mut block_exit = BlockExit::Normal;

        'blockloop: for stmt in prog {
            match stmt {
                Statement::Break => return Ok(BlockExit::Break),
                Statement::Continue => return Ok(BlockExit::Continue),

                Statement::Return(expr) => {
                    let val = self.resolve_expr(expr)?;

                    block_exit = BlockExit::Return(val);
                    break 'blockloop;
                }

                Statement::Expr(expr) => {
                    self.resolve_expr(expr)?;
                }

                Statement::Declaration(decl) => {
                    let rhs = self.resolve_expr(&decl.rhs)?;
                    self.vartable.push(rhs);
                }

                Statement::Block(block) => match self.run_block(block)? {
                    // Propagate return, continue and break
                    be @ (BlockExit::Return(_) | BlockExit::Continue | BlockExit::Break) => {
                        block_exit = be;
                        break 'blockloop;
                    }
                    _ => (),
                },

                Statement::Loop(looop) => {
                    // loop runs as long condition != 0
                    loop {
                        // Check the loop condition
                        if let Some(condition) = &looop.condition {
                            if matches!(self.resolve_expr(condition)?, Value::I64(0)) {
                                break;
                            }
                        }

                        // Run the body
                        let be = self.run_block(&looop.body)?;
                        match be {
                            // Propagate return
                            be @ BlockExit::Return(_) => {
                                block_exit = be;
                                break 'blockloop;
                            }
                            BlockExit::Break => break,
                            BlockExit::Continue | BlockExit::Normal => (),
                        }

                        // Run the advancement
                        if let Some(adv) = &looop.advancement {
                            self.resolve_expr(&adv)?;
                        }
                    }
                }

                Statement::Print(expr) => {
                    let result = self.resolve_expr(expr)?;

                    if self.capture_output {
                        self.output.push(result)
                    } else {
                        print!("{}", self.value_to_string(&result));
                    }
                }

                Statement::If(If {
                    condition,
                    body_true,
                    body_false,
                }) => {
                    // Run the right block depending on the conditions result being 0 or not 
                    let exit = if matches!(self.resolve_expr(condition)?, Value::I64(0)) {
                        self.run_block(body_false)?
                    } else {
                        self.run_block(body_true)?
                    };

                    match exit {
                        // Propagate return, continue and break
                        be @ (BlockExit::Return(_) | BlockExit::Continue | BlockExit::Break) => {
                            block_exit = be;
                            break 'blockloop;
                        }
                        _ => (),
                    }
                }

                Statement::FunDeclare(fundec) => {
                    self.funtable.push(fundec.clone());
                }
            }
        }

        self.vartable.truncate(framepointer);

        Ok(block_exit)
    }

    /// Execute the given expression to retrieve the resulting value
    fn resolve_expr(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        let val = match expr {
            Expression::I64(val) => Value::I64(*val),
            Expression::ArrayLiteral(size) => {
                let size = match self.resolve_expr(size)? {
                    Value::I64(size) if !size.is_negative() => size,
                    val => return Err(RuntimeError::InvalidArrayIndex(val)),
                };
                Value::Array(Rc::new(RefCell::new(vec![Value::I64(0); size as usize])))
            }
            Expression::String(text) => Value::String(text.clone()),
            Expression::BinOp(bo, lhs, rhs) => self.resolve_binop(bo, lhs, rhs)?,
            Expression::UnOp(uo, operand) => self.resolve_unop(uo, operand)?,
            Expression::Var(name, idx) => self.resolve_var(*name, *idx)?,
            Expression::ArrayAccess(name, idx, arr_idx) => {
                self.resolve_array_access(*name, *idx, arr_idx)?
            }

            Expression::FunCall(fun_name, fun_stackpos, args) => {
                let args_len = args.len();

                // All of the arg expressions must be resolved before pushing the vars on the stack,
                // otherwise the stack positions are incorrect while resolving
                let args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg))
                    .collect::<Vec<_>>();
                for arg in args {
                    self.vartable.push(arg?);
                }

                // Function existance has been verified in the parser, so unwrap here shouldn't fail
                let expected_num_args = self.funtable.get(*fun_stackpos).unwrap().argnames.len();

                // Check if the number of provided arguments matches the number of expected arguments
                if expected_num_args != args_len {
                    let fun_name = self
                        .stringstore
                        .lookup(*fun_name)
                        .cloned()
                        .unwrap_or("<unknown>".to_string());
                    return Err(RuntimeError::InvalidNumberOfArgs(
                        fun_name,
                        expected_num_args,
                        args_len,
                    ));
                }

                // Run the function body and return the BlockExit type
                match self.run_block_fp_offset(
                    &Rc::clone(&self.funtable.get(*fun_stackpos).unwrap().body),
                    expected_num_args,
                )? {
                    BlockExit::Normal | BlockExit::Continue | BlockExit::Break => Value::Void,
                    BlockExit::Return(val) => val,
                }
            }
        };

        Ok(val)
    }

    /// Retrive the value of a given array at the specified index from the varstack. The name is
    /// given as a StringID and is used to reference the variable name in case of an error. The
    /// idx is the stackpos where the array variable should be located and the arr_idx is the
    /// actual array access index, given as an expression.
    fn resolve_array_access(
        &mut self,
        name: Sid,
        idx: usize,
        arr_idx: &Expression,
    ) -> Result<Value, RuntimeError> {
        // Resolve the array index into a value and check if it is a valid array index
        let arr_idx = match self.resolve_expr(arr_idx)? {
            Value::I64(size) if !size.is_negative() => size,
            val => return Err(RuntimeError::InvalidArrayIndex(val)),
        };

        // Get the array value
        let val = match self.get_var(idx) {
            Some(val) => val,
            None => {
                return Err(RuntimeError::VarUsedNotDeclared(
                    self.stringstore
                        .lookup(name)
                        .cloned()
                        .unwrap_or_else(|| "<unknown>".to_string()),
                ))
            }
        };

        // Make sure it is an array
        let arr = match val {
            Value::Array(arr) => arr,
            _ => {
                return Err(RuntimeError::TryingToIndexNonArray(
                    self.stringstore
                        .lookup(name)
                        .cloned()
                        .unwrap_or_else(|| "<unknown>".to_string()),
                ))
            }
        };

        // Get the value of the requested cell inside the array
        let arr = arr.borrow();
        arr.get(arr_idx as usize)
            .cloned()
            .ok_or(RuntimeError::ArrayOutOfBounds(arr_idx as usize, arr.len()))
    }

    /// Retrive the value of a given variable from the varstack. The name is given as a StringID
    /// and is used to reference the variable name in case of an error. The idx is the stackpos
    /// where the variable should be located
    fn resolve_var(&mut self, name: Sid, idx: usize) -> Result<Value, RuntimeError> {
        match self.get_var(idx) {
            Some(val) => Ok(val),
            None => {
                return Err(RuntimeError::VarUsedNotDeclared(
                    self.stringstore
                        .lookup(name)
                        .cloned()
                        .unwrap_or_else(|| "<unknown>".to_string()),
                ))
            }
        }
    }

    /// Execute a unary operation and get the resulting value
    fn resolve_unop(&mut self, uo: &UnOpType, operand: &Expression) -> Result<Value, RuntimeError> {
        // Recursively resolve the operands expression into an actual value
        let operand = self.resolve_expr(operand)?;

        // Perform the correct operation, considering the operation and value type
        Ok(match (operand, uo) {
            (Value::I64(val), UnOpType::Negate) => Value::I64(-val),
            (Value::I64(val), UnOpType::BNot) => Value::I64(!val),
            (Value::I64(val), UnOpType::LNot) => Value::I64(if val == 0 { 1 } else { 0 }),
            (val, _) => return Err(RuntimeError::UnOpInvalidType(val)),
        })
    }

    /// Execute a binary operation and get the resulting value
    fn resolve_binop(
        &mut self,
        bo: &BinOpType,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Value, RuntimeError> {
        let rhs = self.resolve_expr(rhs)?;

        // Handle assignments separate from the other binary operations
        match (&bo, &lhs) {
            // Normal variable assignment
            (BinOpType::Assign, Expression::Var(name, idx)) => {
                // Get the variable mutably and assign the right hand side value
                match self.get_var_mut(*idx) {
                    Some(val) => *val = rhs.clone(),
                    None => {
                        return Err(RuntimeError::VarUsedNotDeclared(
                            self.stringstore
                                .lookup(*name)
                                .cloned()
                                .unwrap_or_else(|| "<unknown>".to_string()),
                        ))
                    }
                }

                return Ok(rhs);
            }
            // Array index assignment
            (BinOpType::Assign, Expression::ArrayAccess(name, idx, arr_idx)) => {
                // Calculate the array index
                let arr_idx = match self.resolve_expr(arr_idx)? {
                    Value::I64(size) if !size.is_negative() => size,
                    val => return Err(RuntimeError::InvalidArrayIndex(val)),
                };

                // Get the mutable ref to the array variable
                let val = match self.get_var_mut(*idx) {
                    Some(val) => val,
                    None => {
                        return Err(RuntimeError::VarUsedNotDeclared(
                            self.stringstore
                                .lookup(*name)
                                .cloned()
                                .unwrap_or_else(|| "<unknown>".to_string()),
                        ))
                    }
                };

                // Verify that it actually is an array
                match val {
                    // Assign the right hand side value to the array it the given index
                    Value::Array(arr) => arr.borrow_mut()[arr_idx as usize] = rhs.clone(),
                    _ => {
                        return Err(RuntimeError::TryingToIndexNonArray(
                            self.stringstore
                                .lookup(*name)
                                .cloned()
                                .unwrap_or_else(|| "<unknown>".to_string()),
                        ))
                    }
                }

                return Ok(rhs);
            }
            _ => (),
        }

        // This code is only executed if the binop is not an assignment as the assignments return
        // early

        // Resolve the left hand side to the value
        let lhs = self.resolve_expr(lhs)?;

        // Perform the appropriate calculations considering the operation type and datatypes of the
        // two values
        let result = match (lhs, rhs) {
            (Value::I64(lhs), Value::I64(rhs)) => match bo {
                BinOpType::Add => Value::I64(lhs + rhs),
                BinOpType::Mul => Value::I64(lhs * rhs),
                BinOpType::Sub => Value::I64(lhs - rhs),
                BinOpType::Div => {
                    Value::I64(lhs.checked_div(rhs).ok_or(RuntimeError::DivideByZero)?)
                }
                BinOpType::Mod => {
                    Value::I64(lhs.checked_rem(rhs).ok_or(RuntimeError::DivideByZero)?)
                }
                BinOpType::BOr => Value::I64(lhs | rhs),
                BinOpType::BAnd => Value::I64(lhs & rhs),
                BinOpType::BXor => Value::I64(lhs ^ rhs),
                BinOpType::LAnd => Value::I64(if (lhs != 0) && (rhs != 0) { 1 } else { 0 }),
                BinOpType::LOr => Value::I64(if (lhs != 0) || (rhs != 0) { 1 } else { 0 }),
                BinOpType::Shr => Value::I64(lhs >> rhs),
                BinOpType::Shl => Value::I64(lhs << rhs),
                BinOpType::EquEqu => Value::I64(if lhs == rhs { 1 } else { 0 }),
                BinOpType::NotEqu => Value::I64(if lhs != rhs { 1 } else { 0 }),
                BinOpType::Less => Value::I64(if lhs < rhs { 1 } else { 0 }),
                BinOpType::LessEqu => Value::I64(if lhs <= rhs { 1 } else { 0 }),
                BinOpType::Greater => Value::I64(if lhs > rhs { 1 } else { 0 }),
                BinOpType::GreaterEqu => Value::I64(if lhs >= rhs { 1 } else { 0 }),

                BinOpType::Assign => unreachable!(),
            },
            (lhs, rhs) => return Err(RuntimeError::BinOpIncompatibleTypes(lhs, rhs)),
        };

        Ok(result)
    }

    /// Get a string representation of the given value. This uses the interpreters StringStore to
    /// retrive the text values of Strings
    fn value_to_string(&self, val: &Value) -> String {
        match val {
            Value::I64(val) => format!("{}", val),
            Value::Array(val) => format!("{:?}", val.borrow()),
            Value::String(text) => format!(
                "{}",
                self.stringstore
                    .lookup(*text)
                    .unwrap_or(&"<invalid string>".to_string())
            ),
            Value::Void => format!("void"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Interpreter, Value};
    use crate::ast::{BinOpType, Expression};

    /// Simple test to check if a simple expression is executed properly.
    /// Full system tests from lexing to execution can be found in `lib.rs`
    #[test]
    fn test_interpreter_expr() {
        // Expression: 1 + 2 * 3 + 4
        // With precedence: (1 + (2 * 3)) + 4
        let ast = Expression::BinOp(
            BinOpType::Add,
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
        );

        let expected = Value::I64(11);

        let mut interpreter = Interpreter::new();
        let actual = interpreter.resolve_expr(&ast).unwrap();

        assert_eq!(expected, actual);
    }
}
