use std::{
    env::args,
    fs,
    // io::{stdin, stdout, Write},
    process::exit,
};

use nek_lang::interpreter::Interpreter;

#[derive(Debug, Default)]
struct CliConfig {
    print_tokens: bool,
    print_ast: bool,
    no_optimizations: bool,
    // interactive: bool,
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
            // "--interactive" | "-i" => conf.interactive = true,
            "--help" | "-h" => print_help(),
            file if conf.file.is_none() => conf.file = Some(file.to_string()),
            _ => panic!("Invalid argument: '{}'", arg),
        }
    }

    let mut interpreter = Interpreter::new();

    interpreter.print_tokens = conf.print_tokens;
    interpreter.print_ast = conf.print_ast;
    interpreter.optimize_ast = !conf.no_optimizations;

    if let Some(file) = &conf.file {
        let code = fs::read_to_string(file).expect(&format!("File not found: '{}'", file));
        interpreter.run_str(&code);
    }

    // TODO: The interactive prompt is currently broken due to the precalculated stack positions.
    // For this to still work, there needs to be a way to keep the stack in the interpreter after
    // runing once. Also somehow the stringstore and var stack from the last parsing stages would
    // need to be reused for the parser to work correctly

    // if conf.interactive || conf.file.is_none() {
    //     let mut code = String::new();

    //     loop {
    //         print!(">> ");
    //         stdout().flush().unwrap();

    //         code.clear();
    //         stdin().read_line(&mut code).unwrap();

    //         if code.trim() == "exit" {
    //             break;
    //         }

    //         interpreter.run_str(&code);
    //     }
    // }
}

fn print_help() {
    println!("Usage nek-lang [FLAGS] [FILE]");
    println!("FLAGS: ");
    println!("-t, --token        Print the lexed tokens");
    println!("-a, --ast          Print the abstract syntax tree");
    println!("-n, --no-opt       Disable the AST optimizations");
    // println!("-i, --interactive  Interactive mode");
    println!("-h, --help         Show this help screen");
    exit(0);
}
