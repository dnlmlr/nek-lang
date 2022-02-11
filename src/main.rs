use std::{env::args, fs, process::exit};

use nek_lang::{interpreter::Interpreter, nice_panic};

/// Cli configuration flags and arguments. This could be done with `clap`, but since only so few
/// arguments are supported this seems kind of overkill.
#[derive(Debug, Default)]
struct CliConfig {
    print_tokens: bool,
    print_ast: bool,
    no_optimizations: bool,
    file: Option<String>,
}

fn main() {
    let mut conf = CliConfig::default();

    // Go through all commandline arguments except the first (filename)
    for arg in args().skip(1) {
        match arg.as_str() {
            "--token" | "-t" => conf.print_tokens = true,
            "--ast" | "-a" => conf.print_ast = true,
            "--no-opt" | "-n" => conf.no_optimizations = true,
            "--help" | "-h" => print_help(),
            file if !arg.starts_with("-") && conf.file.is_none() => {
                conf.file = Some(file.to_string())
            }
            _ => nice_panic!("Error: Invalid argument '{}'", arg),
        }
    }

    let mut interpreter = Interpreter::new();

    interpreter.print_tokens = conf.print_tokens;
    interpreter.print_ast = conf.print_ast;
    interpreter.optimize_ast = !conf.no_optimizations;

    if let Some(file) = &conf.file {
        let code = match fs::read_to_string(file) {
            Ok(code) => code,
            Err(_) => nice_panic!("Error: Could not read file '{}'", file),
        };
        // Lex, parse and run the program
        interpreter.run_str(&code);
    } else {
        println!("Error: No file given\n");
        print_help();
    }
}

fn print_help() {
    println!("Usage nek-lang [FLAGS] [FILE]");
    println!("FLAGS: ");
    println!("-t, --token        Print the lexed tokens");
    println!("-a, --ast          Print the abstract syntax tree");
    println!("-n, --no-opt       Disable the AST optimizations");
    println!("-h, --help         Show this help screen");
    exit(0);
}
