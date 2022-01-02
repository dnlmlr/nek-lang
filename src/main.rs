use nek_lang::lexer::lex;


fn main() {

    let code = "33 +5*2";

    let tokens = lex(code);

    println!("{:?}", tokens);

}
