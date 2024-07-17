#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", format!($msg))
        }
    };
    ($self:expr, $msg:expr) => {
        if LOG_DEBUG_INFO {
            println!(
                "[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1 + 1, $self.pos.0 + 1, format!($msg)
            )
        }
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        if LOG_DEBUG_INFO {
            println!(
                "[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1 + 1, $self.pos.0 + 1, format!($fmt, $($arg)+)
            )
        }
    };
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {
        Err(format!("[ERROR_{MSG}] {}", format!($msg)))
    };
    ($self:expr, $msg:expr) => {
        Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}", $self.pos.1 + 1, $self.pos.0 + 1, format!($msg)))
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}",  $self.pos.1 + 1, $self.pos.0 + 1, format!($fmt, $($arg)+)))
    };
}
