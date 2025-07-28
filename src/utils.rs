use crate::{compile_chain::CompileChain, formatting::SHORT_NODE_PRINT};
use std::{
    convert::Infallible,
    fs,
    io::{BufRead, BufReader},
    ops::{ControlFlow, FromResidual, Try},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct Logger {
    file_pos: Pos,
    max_digits: Pos,
    prefix: String,
    padding: String,
    pub print_logs: bool,
    pub print_output: bool,
    pub print_short_nodes: bool,
}

impl Logger {
    pub fn new(
        contents: &Contents,
        print_logs: bool,
        print_output: bool,
        print_short_nodes: bool,
    ) -> Logger {
        Logger {
            prefix: "LEX".to_string(),
            file_pos: Pos { x: 0, y: 0 },
            max_digits: contents.get_max_digits(),
            padding: " ".repeat(10), // if you have more than 10 digits, you're on your own
            print_logs,
            print_output,
            print_short_nodes,
        }
    }

    pub fn get_padding(&self, p: Pos) -> (&str, &str) {
        let x_padding = self.max_digits.x - count_digits(p.x + 1);
        let y_padding = self.max_digits.y - count_digits(p.y + 1);

        (
            self.padding.get(..x_padding as usize).unwrap(),
            self.padding.get(..y_padding as usize).unwrap(),
        )
    }

    pub fn get_pos(&self) -> Pos {
        self.file_pos
    }

    pub fn set_short_fmt(&mut self, state: bool) {
        unsafe {
            if self.print_short_nodes == state {
                return;
            }
            println!("changing short node print to {state}");
            self.print_short_nodes = state;
        }
    }

    pub fn add_pos(&mut self, delta: Pos) {
        if self.print_logs {
            // println!("{:?} + {delta:?}", unsafe { LOGGER.file_pos });
        }

        self.file_pos.x += delta.x;
        self.file_pos.y += delta.y;
    }

    pub fn sub_pos(&mut self, delta: Pos) {
        if self.print_logs {
            // println!("{:?} - {delta:?}", unsafe { LOGGER.file_pos });
        }

        self.file_pos.x -= delta.x;
        self.file_pos.y -= delta.y;
    }

    pub fn set_pos(&mut self, new_pos: Pos) {
        if self.print_logs {
            // println!("{:?} -> {new_pos:?}", unsafe { LOGGER.file_pos });
        }

        self.file_pos = new_pos;
    }

    pub fn set_prefix(&mut self, new_prefix: &str) {
        self.prefix = new_prefix.to_string();
        self.set_pos(pos(0, 0));
        if self.print_logs {
            println!(
                "\n{}\n\n\n",
                text_to_ascii_art::to_art(format!(">{new_prefix}<"), "standard", 8, 0, 0)
                    .expect("[LOGGER] Failed to generate ASCII art")
            );
        }
    }

    pub fn toggle_logs(&mut self, state: bool) {
        if self.print_logs {
            println!(
                "[COMPILER] Logs already {} for {}",
                if state { "enabled" } else { "disabled" },
                self.prefix
            );
            return;
        }

        self.print_logs = state;
        if state {
            println!("[COMPILER] Logs enabled for {}", self.prefix);
        } else {
            println!("[COMPILER] Logs disabled for {}", self.prefix);
        }
    }

    pub fn log_msg_pos(&self) -> String {
        let pos = self.file_pos;
        let (x_padding, y_padding) = self.get_padding(pos);
        format!("[y:{y_padding}{}, x:{x_padding}{}]", pos.y + 1, pos.x + 1)
    }

    pub fn err_msg(&self, msg: String) -> String {
        let pos = self.file_pos;
        let (x_padding, y_padding) = self.get_padding(pos);
        format!("[ERR_{} | {} {msg}", self.prefix, self.log_msg_pos())
    }

    pub fn dbg_msg(&self, msg: String) -> String {
        let pos = self.file_pos;
        let (x_padding, y_padding) = self.get_padding(pos);
        format!("\n{} {msg}", self.log_msg_pos())
    }
}
// endregion
// region: File Contents
#[derive(Debug, Clone)]
pub struct Contents {
    pub src: Vec<String>,
}

impl<'a> Contents {
    pub fn new(src: Vec<String>) -> Self {
        Contents { src }
    }

    pub fn get_max_digits(&self) -> Pos {
        Pos {
            x: count_digits(self.src.iter().map(|x| x.len()).max().unwrap_or(0) as u32),
            y: count_digits(self.src.len() as u32),
        }
    }

    pub fn get_src_oneline(&'a self, start: Pos, end: Pos) -> &'a str {
        match self.src.get(start.y as usize) {
            Some(line) if (end.x as usize) <= line.len() => &line[start.x as usize..end.x as usize],
            _ => {
                // println!(
                //     "[COMPILER] Invalid start position {start:?}, {end:?}.. {}",
                //     self.contents.len()
                // );
                " couldn't get src oneline. "
            }
        }
    }

    pub fn get_src(&'a self, start: Pos, end: Pos) -> Vec<&'a str> {
        if start.y == end.y {
            return vec![self.get_src_oneline(start, end)];
        }

        let mut vec = vec![];
        for i in start.y..=end.y {
            if i == start.y {
                match self.src.get(i as usize) {
                    Some(line) => vec.push(&line[start.x as usize..]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                continue;
            } else if i == end.y {
                match self.src.get(i as usize) {
                    Some(line) => vec.push(&line[..end.x as usize]),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
                break;
            } else {
                match self.src.get(i as usize) {
                    Some(line) => vec.push(line),
                    None => panic!("Invalid start position {start:?}, {end:?}"),
                }
            }
        }
        vec
    }

    pub fn get_lines(&'a self, start_y: u32, end_y: u32) -> Vec<&'a str> {
        let last_line_len = match self.src.get(end_y as usize) {
            Some(line) => line.len().max(1) as u32 - 1,
            None => 0,
        };
        self.get_src(
            Pos { x: 0, y: start_y },
            Pos {
                x: last_line_len,
                y: end_y,
            },
        )
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
// region: macros
#[macro_export]
macro_rules! debug {
    ($this:ident, $fmt:expr, $($arg:tt)+) => {
        $this.logger.dbg_msg(format!($fmt, $($arg)+))
    };
    ($this:ident, $msg:expr) => {
        $this.logger.dbg_msg(format!($msg))
    };
}

#[macro_export]
macro_rules! err {
    ($this:ident, $fmt:expr, $($arg:tt)+) => {
        Err(anyhow::anyhow!($this.logger.err_msg(format!($fmt, $($arg)+))))
    };

    ($this:ident, $msg:expr) => {
        Err(anyhow::anyhow!($this.logger.err_msg(format!($msg))))
    };
}

#[macro_export]
macro_rules! comp_err {
    ($this:ident, ($data:expr), $fmt:expr, $($arg:tt)+) => {
        crate::utils::CompilerResult::Err {
            data: Some($data),
            error: anyhow::anyhow!($this.logger.err_msg(format!($fmt, $($arg)+))),
        }
    };
    ($this:ident, ($data:expr), $msg:expr) => {
        crate::utils::CompilerResult::Err {
            data: Some($data),
            error: anyhow::anyhow!($this.logger.err_msg(format!($msg))),
        }
    };
    ($this:ident, $fmt:expr, $($arg:tt)+) => {
        crate::utils::CompilerResult::Err {
            data: None,
            error: anyhow::anyhow!($this.logger.err_msg(format!($fmt, $($arg)+))),
        }
    };
    ($this:ident, $msg:expr) => {
        crate::utils::CompilerResult::Err {
            data: None,
            error: anyhow::anyhow!($this.logger.err_msg(format!($msg))),
        }
    };
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
