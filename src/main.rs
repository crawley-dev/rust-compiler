use std::path::PathBuf;

use toy_compiler::{
    compile_chain::Globals,
    utils::{self, Contents, Logger},
};

fn main() {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LIB_BACKTRACE", "1");

    // Print Banner
    println!(
        "{}",
        text_to_ascii_art::to_art(">Toy Compiler<".to_string(), "standard", 8, 0, 0).unwrap()
    );

    let file_name = utils::get_cmd_arg(2);
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("examples");
    path.push(&file_name);

    let contents = Contents::new(utils::get_file_contents(&path));
    let logger = Logger::new(&contents, false, false, false);
    let compile_chain = Globals::register_new_compile_chain(Some(&file_name), contents, logger);

    compile_chain.lex().parse().check();
}

// UBUNTU bash script:
// read file
// sudo nasm -felf64 $file.asm -o $file.o
// sudo ld $file.o -o $file
// ./$file
// echo $?

// Usage:
// bash gen.sh
// FILE_NAME
