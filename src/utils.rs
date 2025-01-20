use std::{
    convert::Infallible,
    fs,
    io::{BufRead, BufReader},
    ops::{ControlFlow, FromResidual, Try},
};

use crate::{Node, Scope};

// region: Logger

static mut LOGGER: Logger = Logger {
    log_prefixes: ["LEX", "PARSE", "SEMANTIC", "CODEGEN"],
    print_logs: [false, false, false, false],
    print_output: [false, false, false, false],
    current_prefix: LogPrefix::Lexical,
    file_pos: Pos { x: 0, y: 0 },
    padding: String::new(),
    max_digits: Pos { x: 0, y: 0 },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogPrefix {
    Lexical,
    Parse,
    Semantic,
    CodeGen,
}

pub struct Logger {
    log_prefixes: [&'static str; 4],
    print_logs: [bool; 4],
    print_output: [bool; 4],
    current_prefix: LogPrefix,
    file_pos: Pos,
    padding: String,
    max_digits: Pos,
}

impl Logger {
    pub fn add_pos(delta: Pos) {
        // if print_logs() {
        //     println!("{:?} + {delta:?}", unsafe { LOGGER.file_pos });
        // }

        unsafe {
            LOGGER.file_pos.x += delta.x;
            LOGGER.file_pos.y += delta.y;
        }
    }

    pub fn sub_pos(delta: Pos) {
        // if print_logs() {
        //     println!("{:?} - {delta:?}", unsafe { self.file_pos });
        // }

        unsafe {
            LOGGER.file_pos.x -= delta.x;
            LOGGER.file_pos.y -= delta.y;
        }
    }

    pub fn set_pos(new_pos: Pos) {
        // if print_logs() {
        //     println!("{:?} -> {new_pos:?}", unsafe { self.file_pos });
        // }

        unsafe {
            LOGGER.file_pos = new_pos;
        }
    }

    pub fn get_prefix() -> &'static str {
        unsafe { LOGGER.log_prefixes[LOGGER.current_prefix as usize] }
    }

    pub fn print_logs() -> bool {
        unsafe { LOGGER.print_logs[LOGGER.current_prefix as usize] }
    }

    pub fn print_output() -> bool {
        unsafe { LOGGER.print_output[LOGGER.current_prefix as usize] }
    }

    pub fn get_pos() -> Pos {
        unsafe { LOGGER.file_pos }
    }

    pub fn get_padding(p: Pos) -> (&'static str, &'static str) {
        unsafe {
            let x_padding = LOGGER.max_digits.x - (/*1.0 + */p.x as f64).log10().floor() as u32;
            let y_padding = LOGGER.max_digits.y - (/*1.0 + */p.y as f64).log10().floor() as u32;
            (
                LOGGER.padding.get(..x_padding as usize).unwrap(),
                LOGGER.padding.get(..y_padding as usize).unwrap(),
            )
        }
    }

    pub fn set_prefix(new_prefix: LogPrefix) {
        unsafe {
            LOGGER.current_prefix = new_prefix;
        }
        Self::set_pos(pos(0, 0));
        if Self::print_logs() || Self::print_output() {
            match new_prefix {
                LogPrefix::Lexical => {
                    println!(
                        "\n{}\n\n\n",
                        text_to_ascii_art::to_art(">Lexical<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::Parse => {
                    println!(
                        "\n{}\n\n\n",
                        text_to_ascii_art::to_art(">Parse<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::Semantic => {
                    println!(
                        "\n{}\n\n\n",
                        text_to_ascii_art::to_art(">Semantic<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::CodeGen => {
                    println!(
                        "\n{}\n\n\n",
                        text_to_ascii_art::to_art(">CodeGen<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
            }
        }
    }
}

#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if crate::utils::Logger::print_logs() {
            let pos = crate::utils::Logger::get_pos();
            let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
            println!("[DBG_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($msg)
            )
        }
    };
    ($fmt:expr, $($arg:tt)+) => {
        if crate::utils::Logger::print_logs() {
            let pos = crate::utils::Logger::get_pos();
            let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
            println!(
                "[DBG_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+)
            )
        }
    };
}
// endregion

// region: Global File Contents

static mut SOURCE: Contents = Contents {
    file_name: String::new(),
    contents: Vec::new(),
};
static mut CONTENTS_STATIC_REF: Option<Vec<&'static str>> = None;

pub struct Contents {
    file_name: String,
    contents: Vec<String>,
}

impl Contents {
    pub fn init() {
        let file_name = Self::get_file_name();
        let contents = Self::get_file_contents(&file_name);

        let max_height = contents.len();
        let max_width = contents.iter().map(|x| x.len()).max().unwrap_or(0);
        unsafe {
            LOGGER.padding = " ".repeat(10); // if you have more than 10 digits, you're on your own
            LOGGER.max_digits = pos(
                (max_width as f64).log10().floor() as u32,
                (max_height as f64).log10().floor() as u32,
            );

            SOURCE = Contents {
                file_name,
                contents,
            };
        }
    }

    pub fn get_contents_ref() -> &'static [&'static str] {
        unsafe {
            match &CONTENTS_STATIC_REF {
                Some(refs) => refs.as_slice(),
                None => {
                    let refs = SOURCE
                        .contents
                        .iter()
                        .map(|s| &**s as &'static str)
                        .collect::<Vec<_>>();
                    CONTENTS_STATIC_REF = Some(refs);
                    CONTENTS_STATIC_REF.as_ref().unwrap()
                }
            }
        }
    }

    pub fn get_src_oneline(start: Pos, end: Pos) -> &'static str {
        match Self::get_contents_ref().get(start.y as usize) {
            Some(line) => &line[start.x as usize..end.x as usize],
            None => panic!("Invalid start position {start:?}, {end:?}"),
        }
    }

    pub fn get_src(start: Pos, end: Pos) -> Vec<&'static str> {
        if start.y == end.y {
            return vec![Self::get_src_oneline(start, end)];
        }

        let mut vec = vec![];
        for i in start.y..=end.y {
            if i == start.y {
                match Self::get_contents_ref().get(i as usize) {
                    Some(line) => vec.push(&line[start.x as usize..]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                continue;
            } else if i == end.y {
                match Self::get_contents_ref().get(i as usize) {
                    Some(line) => vec.push(&line[..end.x as usize]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                break;
            } else {
                match Self::get_contents_ref().get(i as usize) {
                    Some(line) => vec.push(line),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
            }
        }
        vec
    }

    fn get_file_name() -> String {
        let args: String = std::env::args().skip(1).take(1).collect();
        assert!(!args.is_empty(), "[COMPILER] No file path given!\n");

        let file_name = args.split('.').take(1).collect::<String>();
        // TODO(TOM): re-enable after testing
        // let extension = args.split('.').skip(1).take(1).collect::<String>();
        // else if extension != "txt" {
        //     panic!("[COMPILER] Invalid file extension, '.txt' only\n")
        // }
        file_name
    }

    fn get_file_contents(file_name: &str) -> Vec<String> {
        let file = fs::File::open(format!("./examples/{file_name}.txt"))
            .unwrap_or_else(|_| panic!("[COMPILER] Error opening file '{file_name}'\n"));
        BufReader::new(file)
            .lines()
            .map(|line| line.unwrap() + "\n")
            .collect()
    }
}

// endregion

// region: Position
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub x: u32,
    pub y: u32,
}

pub fn pos(x: u32, y: u32) -> Pos {
    Pos { x, y }
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(col: {}, row: {})", self.y + 1, self.x + 1)
    }
}

impl Pos {
    pub fn fmt_range(&self, other: Pos) -> String {
        if other.x > self.x && other.y > self.y {
            return format!(
                "(col: {}..{}, row: {}..{})",
                self.y + 1,
                other.y + 1,
                self.x + 1,
                other.x + 1
            );
        } else if other.x > self.x && self.y == other.y {
            return format!(
                "(col: {}, row: {}..{})",
                self.y + 1,
                self.x + 1,
                other.x + 1
            );
        } else if other.y > self.y && self.x == other.x {
            return format!(
                "(col: {}..{}, row: {})",
                self.y + 1,
                other.y + 1,
                self.x + 1,
            );
        } else {
            return format!("{self:?}");
        }
    }
}
// endregion

// region: Errors

/*DOCS
    (～￣▽￣)～ My own custom error handling, for more accurate error traces
    IDEA:
        - the error state optionally carries the data up until the error occurred,
        - Very useful for recursive parsing (My entire compiler), as otherwise i would know the func, no more.
        - Specifically, xxx_top_level() and xxx_scope() functions, gives me all stmts up until the failure, not what container.

    NOTES:
        - The Residual is the data
*/

#[derive(Debug)]
pub enum CompilerResult<T, E = anyhow::Error> {
    Ok(T),
    Err { data: Option<T>, error: E },
}

impl<T> CompilerResult<T> {
    pub fn context(self, msg: &'static str) -> Self {
        match self {
            CompilerResult::Ok(data) => CompilerResult::Ok(data),
            CompilerResult::Err { data, error } => CompilerResult::Err {
                data,
                error: error.context(msg),
            },
        }
    }
}

impl<T> Try for CompilerResult<T> {
    type Output = T;
    type Residual = CompilerResult<T>;

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            CompilerResult::Ok(output) => ControlFlow::Continue(output),
            CompilerResult::Err { data, error } => {
                ControlFlow::Break(CompilerResult::Err { data, error })
            }
        }
    }

    fn from_output(output: Self::Output) -> Self {
        CompilerResult::Ok(output)
    }
}

impl<T> FromResidual<Self> for CompilerResult<T> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        residual
    }
}

impl<T> FromResidual<Result<Infallible, anyhow::Error>> for CompilerResult<T> {
    fn from_residual(residual: Result<Infallible, anyhow::Error>) -> Self {
        CompilerResult::Err {
            data: None,
            error: residual.unwrap_err(),
        }
    }
}

#[macro_export]
macro_rules! comp_err {
    ($msg:expr) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        crate::utils::CompilerResult::Err {
            data: None,
            error: anyhow::anyhow!(
                "[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($msg),
            ),
        }
    }};
    // Wrapping $data:expr in parens to differentiate it from fmt string
    (($data:expr), $msg:expr) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        crate::utils::CompilerResult::Err {
            data: Some($data),
            error: anyhow::anyhow!(
                "[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($msg),
            ),
        }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        crate::utils::CompilerResult::Err {
            data: None,
            error: anyhow::anyhow!(
                "[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+),
            ),
        }
    }};
    (($data:expr), $fmt:expr, $($arg:tt)+) => {{
        crate::utils::CompilerResult::Err {
            data: Some($data),
            error: anyhow::anyhow!(
                "[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+),
            ),
        }
    }};
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        Err(anyhow::anyhow!("[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
            crate::utils::Logger::get_prefix(),
            pos.y + 1,
            pos.x + 1,
            format!($msg)
        ))
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        Err(anyhow::anyhow!("[ERR_{} | (col: {y_padding}{}, row: {x_padding}{})] {}",
            crate::utils::Logger::get_prefix(),
            pos.y + 1,
            pos.x + 1,
            format!($fmt, $($arg)+)
        ))
    }};
}

// endregion
