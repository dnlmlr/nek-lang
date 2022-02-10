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

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Invalid error Index: {}", 0.to_string())]
    InvalidArrayIndex(Value),

    #[error("Variable used but not declared: {0}")]
    VarUsedNotDeclared(String),

    #[error("Can't index into non-array variable: {0}")]
    TryingToIndexNonArray(String),

    #[error("Invalid value type for unary operation: {}", 0.to_string())]
    UnOpInvalidType(Value),

    #[error("Incompatible binary operations. Operands don't match: {} {}", 0.to_string(), 1.to_string())]
    BinOpIncompatibleTypes(Value, Value),

    #[error("Array access out of bounds: Accessed {0}, size is {1}")]
    ArrayOutOfBounds(usize, usize),

    #[error("Division by zero")]
    DivideByZero,

    #[error("Invalid number of arguments for function {0}. Expected {1}, got {2}")]
    InvalidNumberOfArgs(String, usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    I64(i64),
    String(Sid),
    Array(RefCell<Vec<Value>>),
    Void,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockExit {
    Normal,
    Break,
    Continue,
    Return(Value),
}

#[derive(Default)]
pub struct Interpreter {
    pub optimize_ast: bool,

    pub print_tokens: bool,
    pub print_ast: bool,

    pub capture_output: bool,
    output: Vec<Value>,

    // Variable table stores the runtime values of variables
    vartable: Vec<Value>,

    funtable: Vec<FunDecl>,

    stringstore: StringStore,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            optimize_ast: true,
            ..Self::default()
        }
    }

    pub fn output(&self) -> &[Value] {
        &self.output
    }

    fn get_var(&self, idx: usize) -> Option<Value> {
        self.vartable.get(self.vartable.len() - idx - 1).cloned()
    }

    fn get_var_mut(&mut self, idx: usize) -> Option<&mut Value> {
        let idx = self.vartable.len() - idx - 1;
        self.vartable.get_mut(idx)
    }

    pub fn run_str(&mut self, code: &str) {
        let tokens = match lex(code) {
            Ok(tokens) => tokens,
            Err(e) => nice_panic!("Lexing error: {}", e),
        };

        if self.print_tokens {
            println!("Tokens: {:?}", tokens);
        }

        let ast = match parse(tokens) {
            Ok(ast) => ast,
            Err(e) => nice_panic!("Parsing error: {}", e),
        };

        match self.run_ast(ast) {
            Ok(_) => (),
            Err(e) => nice_panic!("Runtime error: {}", e),
        }
    }

    pub fn run_ast(&mut self, mut ast: Ast) -> Result<(), RuntimeError> {
        if self.optimize_ast {
            ast = SimpleAstOptimizer::optimize(ast);
        }

        if self.print_ast {
            println!("{:#?}", ast.main);
        }

        self.stringstore = ast.stringstore;

        self.run_block(&ast.main)?;
        Ok(())
    }

    pub fn run_block(&mut self, prog: &BlockScope) -> Result<BlockExit, RuntimeError> {
        self.run_block_fp_offset(prog, 0)
    }

    pub fn run_block_fp_offset(
        &mut self,
        prog: &BlockScope,
        framepointer_offset: usize,
    ) -> Result<BlockExit, RuntimeError> {
        let framepointer = self.vartable.len() - framepointer_offset;

        for stmt in prog {
            match stmt {
                Statement::Break => return Ok(BlockExit::Break),
                Statement::Continue => return Ok(BlockExit::Continue),

                Statement::Return(expr) => {
                    let val = self.resolve_expr(expr)?;

                    self.vartable.truncate(framepointer);
                    return Ok(BlockExit::Return(val));
                }

                Statement::Expr(expr) => {
                    self.resolve_expr(expr)?;
                }

                Statement::Declaration(_sid, _idx, rhs) => {
                    let rhs = self.resolve_expr(rhs)?;
                    self.vartable.push(rhs);
                }

                Statement::Block(block) => match self.run_block(block)? {
                    // Propagate return, continue and break
                    be @ (BlockExit::Return(_) | BlockExit::Continue | BlockExit::Break) => {
                        self.vartable.truncate(framepointer);
                        return Ok(be);
                    }
                    _ => (),
                },

                Statement::Loop(looop) => {
                    // loop runs as long condition != 0
                    loop {
                        if matches!(self.resolve_expr(&looop.condition)?, Value::I64(0)) {
                            break;
                        }

                        let be = self.run_block(&looop.body)?;
                        match be {
                            // Propagate return
                            be @ BlockExit::Return(_) => {
                                self.vartable.truncate(framepointer);
                                return Ok(be);
                            }
                            BlockExit::Break => break,
                            BlockExit::Continue | BlockExit::Normal => (),
                        }

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
                    let exit = if matches!(self.resolve_expr(condition)?, Value::I64(0)) {
                        self.run_block(body_false)?
                    } else {
                        self.run_block(body_true)?
                    };

                    match exit {
                        // Propagate return, continue and break
                        be @ (BlockExit::Return(_) | BlockExit::Continue | BlockExit::Break) => {
                            self.vartable.truncate(framepointer);
                            return Ok(be);
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

        Ok(BlockExit::Normal)
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        let val = match expr {
            Expression::I64(val) => Value::I64(*val),
            Expression::ArrayLiteral(size) => {
                let size = match self.resolve_expr(size)? {
                    Value::I64(size) if !size.is_negative() => size,
                    val => return Err(RuntimeError::InvalidArrayIndex(val)),
                };
                Value::Array(RefCell::new(vec![Value::I64(0); size as usize]))
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

    fn resolve_array_access(
        &mut self,
        name: Sid,
        idx: usize,
        arr_idx: &Expression,
    ) -> Result<Value, RuntimeError> {
        let arr_idx = match self.resolve_expr(arr_idx)? {
            Value::I64(size) if !size.is_negative() => size,
            val => return Err(RuntimeError::InvalidArrayIndex(val)),
        };

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

        let arr = arr.borrow_mut();
        arr.get(arr_idx as usize)
            .cloned()
            .ok_or(RuntimeError::ArrayOutOfBounds(arr_idx as usize, arr.len()))
    }

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

    fn resolve_unop(&mut self, uo: &UnOpType, operand: &Expression) -> Result<Value, RuntimeError> {
        let operand = self.resolve_expr(operand)?;

        Ok(match (operand, uo) {
            (Value::I64(val), UnOpType::Negate) => Value::I64(-val),
            (Value::I64(val), UnOpType::BNot) => Value::I64(!val),
            (Value::I64(val), UnOpType::LNot) => Value::I64(if val == 0 { 1 } else { 0 }),
            (val, _) => return Err(RuntimeError::UnOpInvalidType(val)),
        })
    }

    fn resolve_binop(
        &mut self,
        bo: &BinOpType,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Value, RuntimeError> {
        let rhs = self.resolve_expr(rhs)?;

        match (&bo, &lhs) {
            (BinOpType::Assign, Expression::Var(name, idx)) => {
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
            (BinOpType::Assign, Expression::ArrayAccess(name, idx, arr_idx)) => {
                let arr_idx = match self.resolve_expr(arr_idx)? {
                    Value::I64(size) if !size.is_negative() => size,
                    val => return Err(RuntimeError::InvalidArrayIndex(val)),
                };

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

                match val {
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

        let lhs = self.resolve_expr(lhs)?;

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
