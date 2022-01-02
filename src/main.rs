use nek_lang::{lexer::lex, parser::parse};


fn main() {

    let code = "33 +5*2";
    // Should produce ast: Add {
    //     lhs: I64(33),
    //     rhs: Mul: {
    //         lhs: I64(5),
    //         rhs: I64(2)
    //     }
    // }

    let tokens = lex(code);

    println!("Tokens: {:?}\n", tokens);

    let ast = parse(tokens);

    println!("Ast: {:#?}", ast);

}
