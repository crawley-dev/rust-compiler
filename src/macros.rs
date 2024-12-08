#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if LOG_DEBUG_INFO {
            println!("\n[DEBUG_{MSG}] {}\n", format!($msg))
        }
    };
    ($self:expr, $msg:expr) => {
        if LOG_DEBUG_INFO {
            println!(
                "\n[DEBUG_{MSG} (col:{}, row:{})] {}\n", $self.pos.1 + 1, $self.pos.0 + 1, format!($msg)
            )
        }
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        if LOG_DEBUG_INFO {
            println!(
                "\n[DEBUG_{MSG} (col:{}, row:{})] {}\n", $self.pos.1 + 1, $self.pos.0 + 1, format!($fmt, $($arg)+)
            )
        }
    };
}

#[macro_export]
macro_rules! debugln {
    ($msg:expr) => {
        if LOG_DEBUG_INFO {
            println!("\n[DEBUG_{MSG}] {}\n", format!($msg))
        }
    };
    ($self:expr, $msg:expr) => {
        if LOG_DEBUG_INFO {
            println!(
                "\n[DEBUG_{MSG} (col:{}, row:{})] {}\n", $self.pos.1 + 1, $self.pos.0 + 1, format!($msg)
            )
        }
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        if LOG_DEBUG_INFO {
            println!(
                "\n[DEBUG_{MSG} (col:{}, row:{})] {}\n", $self.pos.1 + 1, $self.pos.0 + 1, format!($fmt, $($arg)+)
            )
        }
    };
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {
        Err(format!("\n[ERROR_{MSG}] {}\n", format!($msg)))
    };
    ($self:expr, $msg:expr) => {
        Err(format!("\n[ERROR_{MSG} (col:{}, row:{})] {}\n", $self.pos.1 + 1, $self.pos.0 + 1, format!($msg)))
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        Err(format!("\n[ERROR_{MSG} (col:{}, row:{})] {}\n",  $self.pos.1 + 1, $self.pos.0 + 1, format!($fmt, $($arg)+)))
    };
}
