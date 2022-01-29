use std::collections::HashMap;

use crate::parser::{Ast, BinOpType, UnOpType};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    I64(i64),
}

pub struct Interpreter {
    // Variable table stores the runtime values of variables
    vartable: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            vartable: HashMap::new(),
        }
    }

    pub fn run(&mut self, prog: Ast) {
        let result = self.resolve_expr(prog);

        println!("Result = {:?}", result);
    }

    fn resolve_expr(&mut self, expr: Ast) -> Value {
        match expr {
            Ast::I64(val) => Value::I64(val),
            Ast::BinOp(bo, lhs, rhs) => self.resolve_binop(bo, *lhs, *rhs),
            Ast::UnOp(uo, operand) => self.resolve_unop(uo, *operand),
            Ast::Var(name) => self.resolve_var(name),
        }
    }

    fn resolve_var(&mut self, name: String) -> Value {
        match self.vartable.get(&name) {
            Some(val) => val.clone(),
            None => panic!("Variable '{}' used but not declared", name),
        }
    }

    fn resolve_unop(&mut self, uo: UnOpType, operand: Ast) -> Value {
        let operand = self.resolve_expr(operand);

        match (operand, uo) {
            (Value::I64(val), UnOpType::Negate) => Value::I64(-val),
            (Value::I64(val), UnOpType::BNot) => Value::I64(!val),
            // _ => panic!("Value type is not compatible with unary operation"),
        }
    }

    fn resolve_binop(&mut self, bo: BinOpType, lhs: Ast, rhs: Ast) -> Value {
        let rhs = self.resolve_expr(rhs);

        match (&bo, &lhs) {
            (BinOpType::Declare, Ast::Var(name)) => {
                self.vartable.insert(name.clone(), rhs.clone());
                return rhs;
            }
            (BinOpType::Assign, Ast::Var(name)) => {
                match self.vartable.get_mut(name) {
                    Some(val) => *val = rhs.clone(),
                    None => panic!("Runtime Error: Trying to assign value to undeclared variable"),
                }
                return rhs;
            }
            _ => ()
        }
        
        let lhs = self.resolve_expr(lhs);

        match (lhs, rhs) {
            (Value::I64(lhs), Value::I64(rhs)) => match bo {
                BinOpType::Add => Value::I64(lhs + rhs),
                BinOpType::Mul => Value::I64(lhs * rhs),
                BinOpType::Sub => Value::I64(lhs - rhs),
                BinOpType::Div => Value::I64(lhs / rhs),
                BinOpType::Mod => Value::I64(lhs % rhs),
                BinOpType::BOr => Value::I64(lhs | rhs),
                BinOpType::BAnd => Value::I64(lhs & rhs),
                BinOpType::BXor => Value::I64(lhs ^ rhs),
                BinOpType::Shr => Value::I64(lhs >> rhs),
                BinOpType::Shl => Value::I64(lhs << rhs),
                BinOpType::EquEqu => Value::I64(if lhs == rhs { 1 } else { 0 }),
                BinOpType::NotEqu => Value::I64(if lhs != rhs { 1 } else { 0 }),
                BinOpType::Less => Value::I64(if lhs < rhs { 1 } else { 0 }),
                BinOpType::LessEqu => Value::I64(if lhs <= rhs { 1 } else { 0 }),
                BinOpType::Greater => Value::I64(if lhs > rhs { 1 } else { 0 }),
                BinOpType::GreaterEqu => Value::I64(if lhs >= rhs { 1 } else { 0 }),

                BinOpType::Declare | BinOpType::Assign => unreachable!(),
            },
            // _ => panic!("Value types are not compatible"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Interpreter, Value};
    use crate::parser::{Ast, BinOpType};

    #[test]
    fn test_interpreter_expr() {
        // Expression: 1 + 2 * 3 + 4
        // With precedence: (1 + (2 * 3)) + 4
        let ast = Ast::BinOp(
            BinOpType::Add,
            Ast::BinOp(
                BinOpType::Add,
                Ast::I64(1).into(),
                Ast::BinOp(BinOpType::Mul, Ast::I64(2).into(), Ast::I64(3).into()).into(),
            )
            .into(),
            Ast::I64(4).into(),
        );

        let expected = Value::I64(11);

        let mut interpreter = Interpreter::new();
        let actual = interpreter.resolve_expr(ast);

        assert_eq!(expected, actual);
    }
}
