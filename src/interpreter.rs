use crate::parser::{Ast, BinOpType, UnOpType};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    I64(i64),
}

pub struct Interpreter {
    // Runtime storage, for example variables ...
}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
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
        let lhs = self.resolve_expr(lhs);
        let rhs = self.resolve_expr(rhs);

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
