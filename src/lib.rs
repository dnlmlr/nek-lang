pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod stringstore;
pub mod astoptimizer;
pub mod util;

/// A bunch of full program tests using the example code programs as test subjects.
#[cfg(test)]
mod tests {
    use crate::interpreter::{Interpreter, Value};
    use std::fs::read_to_string;

    /// Run a nek program with the given filename from the examples directory and assert the
    /// captured output with the expected result. This only works if the program just outputs one
    /// value as the result
    fn run_example_check_single_i64_output(filename: &str, correct_result: i64) {
        let mut interpreter = Interpreter::new();
        // Enable output capturing. This captures all calls to `print`
        interpreter.capture_output = true;

        // Load and run the given program
        let code = read_to_string(format!("examples/{filename}")).unwrap();
        interpreter.run_str(&code);

        // Compare the captured output with the expected value
        let expected_output = [Value::I64(correct_result)];
        assert_eq!(interpreter.output(), &expected_output);
    }

    #[test]
    fn test_euler1() {
        run_example_check_single_i64_output("euler1.nek", 233168);
    }

    #[test]
    fn test_euler2() {
        run_example_check_single_i64_output("euler2.nek", 4613732);
    }

    #[test]
    fn test_euler3() {
        run_example_check_single_i64_output("euler3.nek", 6857);
    }

    #[test]
    fn test_euler4() {
        run_example_check_single_i64_output("euler4.nek", 906609);
    }

    #[test]
    fn test_euler5() {
        run_example_check_single_i64_output("euler5.nek", 232792560);
    }

    #[test]
    fn test_recursive_fib() {
        run_example_check_single_i64_output("recursive_fib.nek", 832040);
    }

    #[test]
    fn test_functions() {
        run_example_check_single_i64_output("test_functions.nek", 69);
    }
}

