use crate::parser::{Ast, BinOpType};

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
            },
            // _ => panic!("Value types are not compatible"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{Ast, BinOpType};
    use super::{Interpreter, Value};

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
