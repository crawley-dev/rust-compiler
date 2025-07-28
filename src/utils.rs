use std::{
    convert::Infallible,
    fs,
    io::{BufRead, BufReader},
    ops::{ControlFlow, FromResidual, Try},
    path::PathBuf,
};

use crate::formatting::SHORT_NODE_PRINT;

// region: Logger

static mut LOGGER: Logger = Logger::new();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogPrefix {
    Lex,
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
    pub const fn new() -> Logger {
        Logger {
            log_prefixes: ["LEX", "PARSE", "SEM", "GEN"],
            print_logs: [false, false, true, false],
            print_output: [false, true, true, false],
            current_prefix: LogPrefix::Lex,
            file_pos: Pos { x: 0, y: 0 },
            padding: String::new(),
            max_digits: Pos { x: 0, y: 0 },
        }
    }

    pub fn set_short_fmt(_: bool) {
        unsafe {
            SHORT_NODE_PRINT = false;
            return;

            // if SHORT_NODE_PRINT == state {
            //     return;
            // }
            // println!("changing short node print to {state}");
            // SHORT_NODE_PRINT = state;
        }
    }

    pub fn add_pos(delta: Pos) {
        if Logger::print_logs() {
            // println!("{:?} + {delta:?}", unsafe { LOGGER.file_pos });
        }

        unsafe {
            LOGGER.file_pos.x += delta.x;
            LOGGER.file_pos.y += delta.y;
        }
    }

    pub fn sub_pos(delta: Pos) {
        if Logger::print_logs() {
            // println!("{:?} - {delta:?}", unsafe { LOGGER.file_pos });
        }

        unsafe {
            LOGGER.file_pos.x -= delta.x;
            LOGGER.file_pos.y -= delta.y;
        }
    }

    pub fn set_pos(new_pos: Pos) {
        if Logger::print_logs() {
            // println!("{:?} -> {new_pos:?}", unsafe { LOGGER.file_pos });
        }

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
        unsafe {
            let cond = LOGGER.print_output[LOGGER.current_prefix as usize];
            if cond {
                println!(
                    "\n\n{}\n",
                    text_to_ascii_art::to_art(">Output<".to_string(), "standard", 8, 0, 0).unwrap()
                );
            }
            cond
        }
    }

    pub fn get_pos() -> Pos {
        unsafe { LOGGER.file_pos }
    }

    pub fn get_padding(p: Pos) -> (&'static str, &'static str) {
        unsafe {
            let x_padding = LOGGER.max_digits.x - count_digits(p.x + 1);
            let y_padding = LOGGER.max_digits.y - count_digits(p.y + 1);
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
        if Self::print_logs() {
            match new_prefix {
                LogPrefix::Lex => {
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

    pub fn toggle_logs(state: bool) {
        unsafe {
            if LOGGER.print_logs[LOGGER.current_prefix as usize] == state {
                println!(
                    "[COMPILER] Logs already {} for {}",
                    if state { "enabled" } else { "disabled" },
                    LOGGER.log_prefixes[LOGGER.current_prefix as usize]
                );
                return;
            }

            LOGGER.print_logs[LOGGER.current_prefix as usize] = state;
            if state {
                println!(
                    "[COMPILER] Logs enabled for {}",
                    LOGGER.log_prefixes[LOGGER.current_prefix as usize]
                );
            } else {
                println!(
                    "[COMPILER] Logs disabled for {}",
                    LOGGER.log_prefixes[LOGGER.current_prefix as usize]
                );
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
            println!("\n[y:{y_padding}{}, x:{x_padding}{}] {}",
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
                "\n[y:{y_padding}{}, x:{x_padding}{}] {}",
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+)
            )
        }
    };
}
// endregion

// region: Global File Contents
#[derive(Debug, Clone)]
pub struct Contents {
    pub name: String,
    pub contents: Vec<String>,
}

static mut SOURCE: Contents = Contents {
    name: String::new(),
    contents: Vec::new(),
};
static mut CONTENTS_STATIC_REF: Option<Box<[&'static str]>> = None;

impl Contents {
    pub fn init(name: String, contents: Vec<String>) {
        let max_height = contents.len();
        let max_width = contents.iter().map(|x| x.len()).max().unwrap_or(0);
        unsafe {
            Logger::set_pos(pos(0, 0));
            LOGGER.padding = " ".repeat(10); // if you have more than 10 digits, you're on your own
            LOGGER.max_digits = pos(
                count_digits(max_width as u32),
                count_digits(max_height as u32),
            );

            SOURCE = Contents { name, contents };

            CONTENTS_STATIC_REF = Some(
                SOURCE
                    .contents
                    .iter()
                    .map(|s| &**s as &'static str)
                    .collect::<Box<_>>(),
            );
        }
    }

    pub fn get_contents() -> &'static [&'static str] {
        unsafe {
            match CONTENTS_STATIC_REF.as_ref() {
                Some(ref lines) => lines,
                None => panic!("[COMPILER] Contents not initialized, call Contents::init() first"),
            }
        }
    }

    pub fn get_src_oneline(start: Pos, end: Pos) -> &'static str {
        match Self::get_contents().get(start.y as usize) {
            Some(line) if (end.x as usize) <= line.len() => &line[start.x as usize..end.x as usize],
            _ => {
                // println!(
                //     "[COMPILER] Invalid start position {start:?}, {end:?}.. {}",
                //     Self::get_contents().len()
                // );
                " couldn't get src oneline. "
            }
        }
    }

    pub fn get_src(start: Pos, end: Pos) -> Vec<&'static str> {
        if start.y == end.y {
            return vec![Self::get_src_oneline(start, end)];
        }

        let mut vec = vec![];
        for i in start.y..=end.y {
            if i == start.y {
                match Self::get_contents().get(i as usize) {
                    Some(line) => vec.push(&line[start.x as usize..]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                continue;
            } else if i == end.y {
                match Self::get_contents().get(i as usize) {
                    Some(line) => vec.push(&line[..end.x as usize]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                break;
            } else {
                match Self::get_contents().get(i as usize) {
                    Some(line) => vec.push(line),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
            }
        }
        vec
    }

    pub fn get_lines(start_y: u32, end_y: u32) -> Vec<&'static str> {
        let last_line_len;
        unsafe {
            last_line_len = match SOURCE.contents.get(end_y as usize) {
                Some(line) => line.len().max(1) as u32 - 1,
                None => 0,
            }
        }
        Self::get_src(
            Pos { x: 0, y: start_y },
            Pos {
                x: last_line_len,
                y: end_y,
            },
        )
    }

    pub fn get<'a>() -> &'a Contents {
        unsafe {
            if CONTENTS_STATIC_REF.is_none() {
                panic!("[COMPILER] Contents not initialized, call Contents::init() first");
            }
            &SOURCE
        }
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
        write!(f, "(y: {}, x: {})", self.y + 1, self.x + 1)
    }
}

impl Pos {
    pub fn fmt_range(&self, other: Pos) -> String {
        if other.y > self.y && self.x == other.x {
            return format!("(y: {}..{}, x: {})", self.y + 1, other.y + 1, self.x + 1,);
        } else if other.y > self.y {
            return format!(
                "(y: {}..{}, x: {}..{})",
                self.y + 1,
                other.y + 1,
                self.x + 1,
                other.x + 1
            );
        } else if other.x > self.x && self.y == other.y {
            return format!("(y: {}, x: {}..{})", self.y + 1, self.x + 1, other.x + 1);
        } else {
            return format!("I give up {self:?} {other:?}");
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
        - Specifically, xxx_top_level() and xxx_scope() functions, gives me all stmts up until the failure, not what container they are in!

    NOTES:
        - The Residual is the data
*/

#[derive(Debug)]
pub enum CompilerResult<T, E = anyhow::Error> {
    Ok(T),
    Err { data: Option<T>, error: E },
}

impl<T> CompilerResult<T> {
    pub fn is_ok(&self) -> bool {
        match self {
            CompilerResult::Ok(_) => true,
            CompilerResult::Err { .. } => false,
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            CompilerResult::Ok(_) => false,
            CompilerResult::Err { .. } => true,
        }
    }

    pub fn context<M>(self, msg: M) -> Self
    where
        M: std::fmt::Display + std::marker::Sync + std::marker::Send + 'static,
    {
        match self {
            CompilerResult::Ok(_) => self,
            CompilerResult::Err { data, error } => CompilerResult::Err {
                data,
                error: error.context(msg),
            },
        }
    }

    pub fn with_context<F, C>(self, f: F) -> Self
    where
        C: std::fmt::Display + std::marker::Sync + std::marker::Send + 'static,
        F: FnOnce() -> C,
    {
        match self {
            CompilerResult::Ok(_) => self,
            CompilerResult::Err { data, error } => CompilerResult::Err {
                data,
                error: error.context(f()),
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

impl<T> FromResidual<Result<(), anyhow::Error>> for CompilerResult<T> {
    fn from_residual(residual: Result<(), anyhow::Error>) -> Self {
        CompilerResult::Err {
            data: None,
            error: residual.unwrap_err(),
        }
    }
}

#[macro_export]
macro_rules! comp_err {
    (($data:expr), $fmt:expr, $($arg:tt)+) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        crate::utils::CompilerResult::Err {
            data: Some($data),
            error: anyhow::anyhow!(
                "[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+),
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
                "[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
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
                "[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($fmt, $($arg)+),
            ),
        }
    }};

    ($msg:expr) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        crate::utils::CompilerResult::Err {
            data: None,
            error: anyhow::anyhow!(
                "[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
                crate::utils::Logger::get_prefix(),
                pos.y + 1,
                pos.x + 1,
                format!($msg),
            ),
        }
    }};
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        Err(anyhow::anyhow!("[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
            crate::utils::Logger::get_prefix(),
            pos.y + 1,
            pos.x + 1,
            format!($msg)
        ))
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        let pos = crate::utils::Logger::get_pos();
        let (x_padding, y_padding) = crate::utils::Logger::get_padding(pos);
        Err(anyhow::anyhow!("[ERR_{} | (y:{y_padding}{}, x:{x_padding}{})] {}",
            crate::utils::Logger::get_prefix(),
            pos.y + 1,
            pos.x + 1,
            format!($fmt, $($arg)+)
        ))
    }};
}

// This macro will either give you the 'ok' val, or return an error to the function
// The syntax mimics a closure, if the input is an error with data, it will bind the data to a
// variable you name with $upgraded_data. This can then be used in $upgraded_err to return a new type!
#[macro_export]
macro_rules! upgrade_err {
    ($expr:expr, |$upgraded_data:ident| $upgraded_closure:expr) => {{
        match $expr {
            CompilerResult::Ok(val) => val,
            CompilerResult::Err { data: None, error } => {
                return CompilerResult::Err { data: None, error }
            }
            CompilerResult::Err {
                data: Some($upgraded_data),
                error,
            } => {
                return CompilerResult::Err {
                    data: Some($upgraded_closure),
                    error,
                }
            }
        }
    }};
}

// This function will upgrade the input to a new result type,
// if the input is an error with data, it will bind the data to a variable you name with $upgraded_data.
// This can then be used in $upgraded_closure to return a new type!
#[macro_export]
macro_rules! upgrade_result {
    ($expr:expr, |$upgraded_data:ident| $upgraded_closure:expr) => {{
        match $expr {
            CompilerResult::Ok($upgraded_data) => CompilerResult::Ok($upgraded_closure),
            CompilerResult::Err { data: None, error } => {
                return CompilerResult::Err { data: None, error }
            }
            CompilerResult::Err {
                data: Some($upgraded_data),
                error,
            } => {
                return CompilerResult::Err {
                    data: Some($upgraded_closure),
                    error,
                }
            }
        }
    }};
}
// endregion
// region: Misc

pub fn count_digits<T>(mut n: T) -> T
where
    T: Copy + std::cmp::Eq + From<u32> + std::cmp::Ord + std::ops::DivAssign + std::ops::AddAssign,
{
    if n == T::from(0) {
        return T::from(1);
    }
    let mut count = T::from(0);
    while n > T::from(0) {
        count += T::from(1);
        n /= T::from(10);
    }
    count
}

pub fn get_cmd_arg(arg_position: usize) -> String {
    if arg_position == 1 {
        panic!("[COMPILER] arg position must be at least 1");
    }
    let args = std::env::args()
        .skip(arg_position - 1)
        .take(1)
        .collect::<String>();
    assert!(!args.is_empty(), "[COMPILER] No file path given!\n");

    args
    // let file_name = args.split('.').take(1).collect::<String>();
    // let extension = args.split('.').last().unwrap_or("");
}

// TODO(TOM): use std::fs::read_to_string instead
pub fn get_file_contents(path: &PathBuf) -> Vec<String> {
    let file =
        fs::File::open(&path).unwrap_or_else(|_| panic!("[COMPILER] Error opening '{path:?}'\n"));
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap() + "\n")
        .collect()
}

// endregion
