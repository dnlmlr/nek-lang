use std::{
    env::args,
    fs,
    io::{stdin, stdout, Write},
};

use nek_lang::interpreter::Interpreter;

#[derive(Debug, Default)]
struct CliConfig {
    print_tokens: bool,
    print_ast: bool,
    interactive: bool,
    file: Option<String>,
}

fn main() {
    let mut conf = CliConfig::default();

    // Go through all commandline arguments except the first (filename)
    for arg in args().skip(1) {
        match arg.as_str() {
            "--token" | "-t" => conf.print_tokens = true,
            "--ast" | "-a" => conf.print_ast = true,
            "--interactive" | "-i" => conf.interactive = true,
            file if conf.file.is_none() => conf.file = Some(file.to_string()),
            _ => panic!("Invalid argument: '{}'", arg),
        }
    }

    let mut interpreter = Interpreter::new();

    if let Some(file) = &conf.file {
        let code = fs::read_to_string(file).expect(&format!("File not found: '{}'", file));
        interpreter.run_str(&code, conf.print_tokens, conf.print_ast);
    }

    if conf.interactive || conf.file.is_none() {
        let mut code = String::new();

        loop {
            print!(">> ");
            stdout().flush().unwrap();

            code.clear();
            stdin().read_line(&mut code).unwrap();

            if code.trim() == "exit" {
                break;
            }

            interpreter.run_str(&code, conf.print_tokens, conf.print_ast);
        }
    }
}
