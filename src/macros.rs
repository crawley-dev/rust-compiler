#[macro_export]
macro_rules! err {
    ($msg:expr) => {{
        Err(format!("[ERROR_{MSG}] {}", format!($msg)))
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        Err(format!("[ERROR_{MSG}] {}", format!($fmt, $($arg)+)))
    }};
}

// TODO(TOM): self.pos << most recent token's position. make built in to debug, err messages
#[macro_export]
macro_rules! debug {
    ($msg:expr) => {{
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", format!($msg))
        }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", format!($fmt, $($arg)+))
        }
    }};
}
