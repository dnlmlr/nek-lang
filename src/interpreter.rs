use std::{collections::HashMap, fmt::Display};

use crate::{parser::{Expression, BinOpType, UnOpType, Ast, Statement, parse, If}, lexer::lex};

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

    pub fn run_str(&mut self, code: &str, print_tokens: bool, print_ast: bool) {
        let tokens = lex(code);
        if print_tokens {
            println!("Tokens: {:?}", tokens);
        }

        let ast = parse(tokens);
        if print_ast {
            println!("{:#?}", ast);
        }

        self.run(&ast);
    }

    pub fn run(&mut self, prog: &Ast) {
        for stmt in &prog.prog {
            match stmt {
                Statement::Expr(expr) => {
                    self.resolve_expr(expr);
                }

                Statement::Loop(looop) => {
                    // loop runs as long condition != 0
                    loop {
                        if matches!(self.resolve_expr(&looop.condition), Value::I64(0)) {
                            break;
                        }

                        self.run(&looop.body);

                        if let Some(adv) = &looop.advancement {
                            self.resolve_expr(&adv);
                        }
                    }
                }

                Statement::Print(expr) => {
                    let result = self.resolve_expr(expr);
                    println!("{}", result);
                }

                Statement::If(If {condition, body_true, body_false}) => {
                    if matches!(self.resolve_expr(condition), Value::I64(0)) {
                        self.run(body_false);
                    } else {
                        self.run(body_true);
                    }
                }
            }

        }
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::I64(val) => Value::I64(*val),
            Expression::BinOp(bo, lhs, rhs) => self.resolve_binop(bo, lhs, rhs),
            Expression::UnOp(uo, operand) => self.resolve_unop(uo, operand),
            Expression::Var(name) => self.resolve_var(name),
        }
    }

    fn resolve_var(&mut self, name: &str) -> Value {
        match self.vartable.get(name) {
            Some(val) => val.clone(),
            None => panic!("Variable '{}' used but not declared", name),
        }
    }

    fn resolve_unop(&mut self, uo: &UnOpType, operand: &Expression) -> Value {
        let operand = self.resolve_expr(operand);

        match (operand, uo) {
            (Value::I64(val), UnOpType::Negate) => Value::I64(-val),
            (Value::I64(val), UnOpType::BNot) => Value::I64(!val),
            // _ => panic!("Value type is not compatible with unary operation"),
        }
    }

    fn resolve_binop(&mut self, bo: &BinOpType, lhs: &Expression, rhs: &Expression) -> Value {
        let rhs = self.resolve_expr(rhs);

        match (&bo, &lhs) {
            (BinOpType::Declare, Expression::Var(name)) => {
                self.vartable.insert(name.clone(), rhs.clone());
                return rhs;
            }
            (BinOpType::Assign, Expression::Var(name)) => {
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I64(val) => write!(f, "{}", val),
        }
    }
}


#[cfg(test)]
mod test {
    use super::{Interpreter, Value};
    use crate::parser::{Expression, BinOpType};

    #[test]
    fn test_interpreter_expr() {
        // Expression: 1 + 2 * 3 + 4
        // With precedence: (1 + (2 * 3)) + 4
        let ast = Expression::BinOp(
            BinOpType::Add,
            Expression::BinOp(
                BinOpType::Add,
                Expression::I64(1).into(),
                Expression::BinOp(BinOpType::Mul, Expression::I64(2).into(), Expression::I64(3).into()).into(),
            )
            .into(),
            Expression::I64(4).into(),
        );

        let expected = Value::I64(11);

        let mut interpreter = Interpreter::new();
        let actual = interpreter.resolve_expr(&ast);

        assert_eq!(expected, actual);
    }
}
