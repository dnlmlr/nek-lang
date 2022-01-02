use nek_lang::{lexer::lex, parser::parse, interpreter::Interpreter};


fn main() {

    let mut code = String::new();

    std::io::stdin().read_line(&mut code).unwrap();
    let code = code.trim();

    let tokens = lex(&code);

    println!("Tokens: {:?}\n", tokens);

    let ast = parse(tokens);

    println!("Ast: {:#?}\n", ast);

    let mut interpreter = Interpreter::new();

    interpreter.run(ast);

}
