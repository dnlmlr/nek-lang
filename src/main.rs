use nek_lang::{lexer::lex, parser::parse, interpreter::Interpreter};


fn main() {

    let mut interpreter = Interpreter::new();

    // let mut code = String::new();
    let code = "
        a <- 5;
        // nek-lang best lang
        a * 2;
    ";

    // loop {
        // print!(">> ");
        // std::io::stdout().flush().unwrap();

        // code.clear();
        // std::io::stdin().read_line(&mut code).unwrap();
        // let code = code.trim();

        let tokens = lex(&code);

        println!("Tokens: {:?}\n", tokens);

        let ast = parse(tokens);

        println!("Ast: {:#?}\n", ast);

        interpreter.run(ast);
    // }

}
