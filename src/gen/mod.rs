use crate::{
    checker::CheckedData,
    utils::{CompilerResult, Contents, Logger},
};

// The second attempt at code generation:
// going to first target c code, and compile that first
// then I'll have a reference assembly output to compare against my own codegen later.
pub struct CodeGen<'a> {
    pub contents: &'a Contents,
    pub logger: &'a mut Logger,
}

impl<'a> CodeGen<'a> {
    pub fn new(contents: &'a Contents, logger: &'a mut Logger) -> Self {
        Self { contents, logger }
    }

    pub fn generate_code(&mut self, checked_data: CheckedData) -> CompilerResult<String> {
        let cur_timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis();
        let mut file = std::fs::File::create(format!("/output/{cur_timestamp}.c"))
            .expect("cannot create file?");

        for stmt in &checked_data.ast.stmts {
            // generate some c code.
        }

        return CompilerResult::Ok(String::from("// Generated code placeholder"));
    }
}
