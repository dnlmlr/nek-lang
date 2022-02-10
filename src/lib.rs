pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod stringstore;
pub mod astoptimizer;
pub mod util;

#[cfg(test)]
mod tests {
    use crate::interpreter::{Interpreter, Value};
    use std::fs::read_to_string;

    #[test]
    fn test_euler1() {
        let filename = "euler1.nek";
        let correct_result = 233168;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_euler2() {
        let filename = "euler2.nek";
        let correct_result = 4613732;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_euler3() {
        let filename = "euler3.nek";
        let correct_result = 6857;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_euler4() {
        let filename = "euler4.nek";
        let correct_result = 906609;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_euler5() {
        let filename = "euler5.nek";
        let correct_result = 232792560;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_recursive_fib() {
        
        let filename = "recursive_fib.nek";
        let correct_result = 832040;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_functions() {
        
        let filename = "test_functions.nek";
        let correct_result = 69;

        let mut interpreter = Interpreter::new();
        interpreter.capture_output = true;

        let code = read_to_string(format!("examples/{filename}")).unwrap();

        interpreter.run_str(&code);

        let expected_output = [Value::I64(correct_result)];

        assert_eq!(interpreter.output(), &expected_output);
    }
}

