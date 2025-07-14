use std::{collections::VecDeque, path::PathBuf};

use toy_compiler::{
    compile_chain::Lexable,
    lexer::{token, TokenKind},
    utils::{self, pos, Contents},
};

fn init_example_1() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("examples");
    path.push("example0.txt");

    let file_contents = utils::get_file_contents(&path);
    Contents::init("test1_example0".to_string(), file_contents);
}

#[test]
fn test_1() {
    init_example_1();

    let lexed_contents = Contents::get().lex();
    pretty_assertions::assert_eq!(
        lexed_contents.0,
        VecDeque::from([
            token(TokenKind::Let, pos(0, 0), 3),
            token(TokenKind::Ident, pos(4, 0), 1),
            token(TokenKind::Eq, pos(6, 0), 1),
            token(TokenKind::IntLit, pos(8, 0), 1),
            token(TokenKind::SemiColon, pos(9, 0), 1),
        ]),
    );
}
