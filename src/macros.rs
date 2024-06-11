#[macro_export]
macro_rules! err {
    ($msg:expr) => {{
        Err(format!("{ERR_MSG} {}", format!($msg)))
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        Err(format!("{ERR_MSG} {}", format!($fmt, $($arg)+)))
    }};
}

#[macro_export]
macro_rules! debug {
    ($msg:expr) => {{
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} {}", format!($msg))
        }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        if LOG_DEBUG_INFO {
            println!("{DBG_MSG} {}", format!($fmt, $($arg)+))
        }
    }};
}
