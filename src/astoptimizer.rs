use crate::ast::{Ast, BlockScope, Expression, If, Loop, Statement, BinOpType, UnOpType};

pub trait AstOptimizer {
    fn optimize(ast: Ast) -> Ast;
}

pub struct SimpleAstOptimizer;

impl AstOptimizer for SimpleAstOptimizer {
    fn optimize(mut ast: Ast) -> Ast {
        Self::optimize_block(&mut ast.main);
        ast
    }
}

impl SimpleAstOptimizer {
    fn optimize_block(block: &mut BlockScope) {
        for stmt in block {
            match stmt {
                Statement::Expr(expr) => Self::optimize_expr(expr),
                Statement::Block(block) => Self::optimize_block(block),
                Statement::Loop(Loop {
                    condition,
                    advancement,
                    body,
                }) => {
                    Self::optimize_expr(condition);
                    if let Some(advancement) = advancement {
                        Self::optimize_expr(advancement)
                    }
                    Self::optimize_block(body);
                }
                Statement::If(If {
                    condition,
                    body_true,
                    body_false,
                }) => {
                    Self::optimize_expr(condition);
                    Self::optimize_block(body_true);
                    Self::optimize_block(body_false);
                }
                Statement::Print(expr) => Self::optimize_expr(expr),
            }
        }
    }

    fn optimize_expr(expr: &mut Expression) {
        match expr {
            Expression::BinOp(bo, lhs, rhs) => {
                Self::optimize_expr(lhs);
                Self::optimize_expr(rhs);

                // Precalculate binary operations that consist of 2 literals. No need to do this at 
                // runtime, as all parts of the calculation are known at *compiletime* / parsetime.
                match (lhs.as_mut(), rhs.as_mut()) {
                    (Expression::I64(lhs), Expression::I64(rhs)) => {
                        let new_expr = match bo {
                            BinOpType::Add => Expression::I64(*lhs + *rhs),
                            BinOpType::Mul => Expression::I64(*lhs * *rhs),
                            BinOpType::Sub => Expression::I64(*lhs - *rhs),
                            BinOpType::Div => Expression::I64(*lhs / *rhs),
                            BinOpType::Mod => Expression::I64(*lhs % *rhs),
                            BinOpType::BOr => Expression::I64(*lhs | *rhs),
                            BinOpType::BAnd => Expression::I64(*lhs & *rhs),
                            BinOpType::BXor => Expression::I64(*lhs ^ *rhs),
                            BinOpType::LAnd => Expression::I64(if (*lhs != 0) && (*rhs != 0) { 1 } else { 0 }),
                            BinOpType::LOr => Expression::I64(if (*lhs != 0) || (*rhs != 0) { 1 } else { 0 }),
                            BinOpType::Shr => Expression::I64(*lhs >> *rhs),
                            BinOpType::Shl => Expression::I64(*lhs << *rhs),
                            BinOpType::EquEqu => Expression::I64(if lhs == rhs { 1 } else { 0 }),
                            BinOpType::NotEqu => Expression::I64(if lhs != rhs { 1 } else { 0 }),
                            BinOpType::Less => Expression::I64(if lhs < rhs { 1 } else { 0 }),
                            BinOpType::LessEqu => Expression::I64(if lhs <= rhs { 1 } else { 0 }),
                            BinOpType::Greater => Expression::I64(if lhs > rhs { 1 } else { 0 }),
                            BinOpType::GreaterEqu => Expression::I64(if lhs >= rhs { 1 } else { 0 }),
            
                            BinOpType::Declare | BinOpType::Assign => unreachable!(),
                        };
                        *expr = new_expr;
                    },
                    _ => ()
                }

            }
            Expression::UnOp(uo, operand) => {
                Self::optimize_expr(operand);

                // Precalculate unary operations just like binary ones
                match operand.as_mut() {
                    Expression::I64(val) => {
                        let new_expr = match uo {
                            UnOpType::Negate => Expression::I64(-*val),
                            UnOpType::BNot => Expression::I64(!*val),
                            UnOpType::LNot => Expression::I64(if *val == 0 { 1 } else { 0 }),
                        };
                        *expr = new_expr;
                    }
                    _ => (),
                }
            }
            _ => (),
        }

    }
}
