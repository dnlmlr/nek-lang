use crate::{
    ast::{BlockScope, BinOpType, Expression, If, Statement, UnOpType, Ast},
    lexer::lex,
    parser::parse, stringstore::{Sid, StringStore}, astoptimizer::{SimpleAstOptimizer, AstOptimizer},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    I64(i64),
    String(Sid),
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

    stringstore: StringStore,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { optimize_ast: true, ..Self::default() }
    }

    pub fn output(&self) -> &[Value] {
        &self.output
    }

    fn get_var(&self, idx: usize) -> Option<Value> {
        self.vartable.get(idx).cloned()
    }

    fn get_var_mut(&mut self, idx: usize) -> Option<&mut Value> {
        self.vartable.get_mut(idx)
    }

    pub fn run_str(&mut self, code: &str) {
        let tokens = lex(code).unwrap();
        if self.print_tokens {
            println!("Tokens: {:?}", tokens);
        }

        let ast = parse(tokens);

        self.run_ast(ast);
    }

    pub fn run_ast(&mut self, mut ast: Ast) {
        if self.optimize_ast {
            ast = SimpleAstOptimizer::optimize(ast);
        }
        
        if self.print_ast {
            println!("{:#?}", ast.main);
        }

        self.stringstore = ast.stringstore;

        self.run_block(&ast.main);
    }

    pub fn run_block(&mut self, prog: &BlockScope) {
        let framepointer = self.vartable.len();

        for stmt in prog {
            match stmt {
                Statement::Expr(expr) => {
                    self.resolve_expr(expr);
                }

                Statement::Block(block) => {
                    self.run_block(block);
                }

                Statement::Loop(looop) => {
                    // loop runs as long condition != 0
                    loop {
                        if matches!(self.resolve_expr(&looop.condition), Value::I64(0)) {
                            break;
                        }

                        self.run_block(&looop.body);

                        if let Some(adv) = &looop.advancement {
                            self.resolve_expr(&adv);
                        }
                    }
                }

                Statement::Print(expr) => {
                    let result = self.resolve_expr(expr);

                    if self.capture_output {
                        self.output.push(result)
                    } else {
                        self.print_value(&result);
                    }
                }

                Statement::If(If {
                    condition,
                    body_true,
                    body_false,
                }) => {
                    if matches!(self.resolve_expr(condition), Value::I64(0)) {
                        self.run_block(body_false);
                    } else {
                        self.run_block(body_true);
                    }
                }
            }
        }

        self.vartable.truncate(framepointer);
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::I64(val) => Value::I64(*val),
            Expression::String(text) => Value::String(text.clone()),
            Expression::BinOp(bo, lhs, rhs) => self.resolve_binop(bo, lhs, rhs),
            Expression::UnOp(uo, operand) => self.resolve_unop(uo, operand),
            Expression::Var(name, idx) => self.resolve_var(*name, *idx),
        }
    }

    fn resolve_var(&mut self, name: Sid, idx: usize) -> Value {
        match self.get_var(idx) {
            Some(val) => val,
            None => panic!("Variable '{}' used but not declared", self.stringstore.lookup(name).unwrap()),
        }
    }

    fn resolve_unop(&mut self, uo: &UnOpType, operand: &Expression) -> Value {
        let operand = self.resolve_expr(operand);

        match (operand, uo) {
            (Value::I64(val), UnOpType::Negate) => Value::I64(-val),
            (Value::I64(val), UnOpType::BNot) => Value::I64(!val),
            (Value::I64(val), UnOpType::LNot) => Value::I64(if val == 0 { 1 } else { 0 }),
            _ => panic!("Value type is not compatible with unary operation"),
        }
    }

    fn resolve_binop(&mut self, bo: &BinOpType, lhs: &Expression, rhs: &Expression) -> Value {
        let rhs = self.resolve_expr(rhs);

        match (&bo, &lhs) {
            (BinOpType::Declare, Expression::Var(_name, _idx)) => {
                self.vartable.push(rhs.clone());
                return rhs;
            }
            (BinOpType::Assign, Expression::Var(name, idx)) => {
                match self.get_var_mut(*idx) {
                    Some(val) => *val = rhs.clone(),
                    None => panic!("Runtime Error: Trying to assign value to undeclared variable: {:?}", self.stringstore.lookup(*name)),
                }
                return rhs;
            }
            _ => (),
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

                BinOpType::Declare | BinOpType::Assign => unreachable!(),
            },
            _ => panic!("Value types are not compatible"),
        }
    }

    fn print_value(&self, val: &Value) {
        match val {
            Value::I64(val) => print!("{}", val),
            Value::String(text) => print!("{}", self.stringstore.lookup(*text).unwrap()),
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
        let actual = interpreter.resolve_expr(&ast);

        assert_eq!(expected, actual);
    }
}
