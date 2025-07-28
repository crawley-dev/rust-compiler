use std::{collections::VecDeque, path::PathBuf};

use toy_compiler::{
    compile_chain::Lexable,
    lexer::{token, TokenKind},
    utils::{self, pos, Contents},
};

fn init_example<'a>(id: i32) -> &'a Contents {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("examples");
    path.push(format!("example_{id}.txt"));

    let file_contents = utils::get_file_contents(&path);
    Contents::init(format!("TEST_example_{id}"), file_contents);

    let test = Contents::get();

    println!("[TEST] File contents initialized:\n{:#?}", test);
    test
}

mod lex_tests {
    use super::*;
    #[test]
    fn test_1() {
        let lexed_contents = init_example(1).lex();

        pretty_assertions::assert_eq!(
            lexed_contents.0,
            VecDeque::from([
                token(TokenKind::Fn, pos(0, 0), 2),
                token(TokenKind::Ident, pos(3, 0), 5),
                token(TokenKind::OpenParen, pos(8, 0), 1),
                token(TokenKind::CloseParen, pos(9, 0), 1),
                token(TokenKind::Arrow, pos(11, 0), 2),
                token(TokenKind::Ident, pos(14, 0), 3),
                token(TokenKind::OpenBrace, pos(18, 0), 1),
                token(TokenKind::Return, pos(4, 1), 6),
                token(TokenKind::IntLit, pos(11, 1), 1),
                token(TokenKind::Add, pos(13, 1), 1),
                token(TokenKind::IntLit, pos(15, 1), 1),
                token(TokenKind::Mul, pos(17, 1), 1),
                token(TokenKind::IntLit, pos(19, 1), 1),
                token(TokenKind::Sub, pos(21, 1), 1),
                token(TokenKind::IntLit, pos(23, 1), 1),
                token(TokenKind::Quo, pos(25, 1), 1),
                token(TokenKind::IntLit, pos(27, 1), 1),
                token(TokenKind::SemiColon, pos(28, 1), 1),
                token(TokenKind::CloseBrace, pos(0, 2), 1),
            ]),
        );
    }

    #[test]
    fn test_2() {
        let lexed_contents = init_example(2).lex();

        pretty_assertions::assert_eq!(
            lexed_contents.0,
            VecDeque::from([
                token(TokenKind::Fn, pos(0, 0), 2),
                token(TokenKind::Ident, pos(3, 0), 5),
                token(TokenKind::OpenParen, pos(8, 0), 1),
                token(TokenKind::CloseParen, pos(9, 0), 1),
                token(TokenKind::Arrow, pos(11, 0), 2),
                token(TokenKind::Ident, pos(14, 0), 3),
                token(TokenKind::OpenBrace, pos(18, 0), 1),
                token(TokenKind::Let, pos(4, 1), 3),
                token(TokenKind::Ident, pos(8, 1), 1),
                token(TokenKind::Colon, pos(9, 1), 1),
                token(TokenKind::Ident, pos(11, 1), 3),
                token(TokenKind::SemiColon, pos(14, 1), 1),
                token(TokenKind::Let, pos(4, 2), 3),
                token(TokenKind::Ident, pos(8, 2), 1),
                token(TokenKind::Colon, pos(9, 2), 1),
                token(TokenKind::Ident, pos(11, 2), 3),
                token(TokenKind::SemiColon, pos(14, 2), 1),
                token(TokenKind::Ident, pos(4, 3), 1),
                token(TokenKind::Eq, pos(6, 3), 1),
                token(TokenKind::IntLit, pos(8, 3), 1),
                token(TokenKind::SemiColon, pos(9, 3), 1),
                token(TokenKind::Ident, pos(4, 4), 1),
                token(TokenKind::Eq, pos(6, 4), 1),
                token(TokenKind::Ident, pos(8, 4), 1),
                token(TokenKind::Mul, pos(10, 4), 1),
                token(TokenKind::IntLit, pos(12, 4), 1),
                token(TokenKind::Add, pos(14, 4), 1),
                token(TokenKind::OpenParen, pos(16, 4), 1),
                token(TokenKind::Ident, pos(17, 4), 1),
                token(TokenKind::Sub, pos(19, 4), 1),
                token(TokenKind::IntLit, pos(21, 4), 1),
                token(TokenKind::CloseParen, pos(22, 4), 1),
                token(TokenKind::SemiColon, pos(23, 4), 1),
                token(TokenKind::Return, pos(4, 5), 6),
                token(TokenKind::Ident, pos(11, 5), 1),
                token(TokenKind::Sub, pos(13, 5), 1),
                token(TokenKind::Ident, pos(15, 5), 1),
                token(TokenKind::SemiColon, pos(16, 5), 1),
                token(TokenKind::CloseBrace, pos(0, 6), 1),
            ]),
        );
    }

    #[test]
    fn test_3() {
        let lexed_contents = init_example(3).lex();

        pretty_assertions::assert_eq!(
            lexed_contents.0,
            VecDeque::from([
                // ==== fn test3() -> i32 { ====
                token(TokenKind::Fn, pos(0, 0), 2),
                token(TokenKind::Ident, pos(3, 0), 5), // "test3"
                token(TokenKind::OpenParen, pos(8, 0), 1),
                token(TokenKind::CloseParen, pos(9, 0), 1),
                token(TokenKind::Arrow, pos(11, 0), 2),
                token(TokenKind::Ident, pos(14, 0), 3), // "i32"
                token(TokenKind::OpenBrace, pos(18, 0), 1),
                // let result: i32 = add(10, 20);
                token(TokenKind::Let, pos(4, 1), 3),
                token(TokenKind::Ident, pos(8, 1), 6), // "result"
                token(TokenKind::Colon, pos(14, 1), 1),
                token(TokenKind::Ident, pos(16, 1), 3), // "i32"
                token(TokenKind::Eq, pos(20, 1), 1),
                token(TokenKind::Ident, pos(22, 1), 3), // "add"
                token(TokenKind::OpenParen, pos(25, 1), 1),
                token(TokenKind::IntLit, pos(26, 1), 2), // "10"
                token(TokenKind::Comma, pos(28, 1), 1),
                token(TokenKind::IntLit, pos(30, 1), 2), // "20"
                token(TokenKind::CloseParen, pos(32, 1), 1),
                token(TokenKind::SemiColon, pos(33, 1), 1),
                // return result;
                token(TokenKind::Return, pos(4, 2), 6),
                token(TokenKind::Ident, pos(11, 2), 6), // "result"
                token(TokenKind::SemiColon, pos(17, 2), 1),
                // } of test3
                token(TokenKind::CloseBrace, pos(0, 3), 1),
                // ==== fn add(x: i32, y: i32) -> i32 { ====
                token(TokenKind::Fn, pos(0, 5), 2),
                token(TokenKind::Ident, pos(3, 5), 3), // "add"
                token(TokenKind::OpenParen, pos(6, 5), 1),
                token(TokenKind::Ident, pos(7, 5), 1), // "x"
                token(TokenKind::Colon, pos(8, 5), 1),
                token(TokenKind::Ident, pos(10, 5), 3), // "i32"
                token(TokenKind::Comma, pos(13, 5), 1),
                token(TokenKind::Ident, pos(15, 5), 1), // "y"
                token(TokenKind::Colon, pos(16, 5), 1),
                token(TokenKind::Ident, pos(18, 5), 3), // "i32"
                token(TokenKind::CloseParen, pos(21, 5), 1),
                token(TokenKind::Arrow, pos(23, 5), 2),
                token(TokenKind::Ident, pos(26, 5), 3), // "i32"
                token(TokenKind::OpenBrace, pos(30, 5), 1),
                // return x + y;
                token(TokenKind::Return, pos(4, 6), 6),
                token(TokenKind::Ident, pos(11, 6), 1), // "x"
                token(TokenKind::Add, pos(13, 6), 1),
                token(TokenKind::Ident, pos(15, 6), 1), // "y"
                token(TokenKind::SemiColon, pos(16, 6), 1),
                // } of add
                token(TokenKind::CloseBrace, pos(0, 7), 1),
            ]),
        );
    }
}
