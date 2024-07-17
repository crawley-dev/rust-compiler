#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", $msg)
        }
    };
    ($self:expr, $msg:expr) => {
        if LOG_DEBUG_INFO {
            println!(
                "[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, $msg
            )
        }
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        if LOG_DEBUG_INFO {
            println!(
                "[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, format!($fmt, $($arg)+)
            )
        }
    };
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {
        Err(format!("[ERROR_{MSG}] {}", $msg))
    };
    ($self:expr, $msg:expr) => {
        Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.1, $msg))
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}",  $self.pos.1, $self.pos.0, format!($fmt, $($arg)+)))
    };
}
