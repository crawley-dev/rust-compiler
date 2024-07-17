#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", $msg)
        }
    };
    ($fmt:expr, $($arg:tt)+) => {
        if LOG_DEBUG_INFO {
            println!("[DEBUG_{MSG}] {}", format!($fmt, $($arg)+))
        }
    };
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {
        Err(format!("[ERROR_{MSG}] {}", $msg))
    };
    // ($fmt:expr, $($arg:tt)+) => {
    //     Err(format!("[ERROR_{MSG}] {}", format!($fmt, $($arg)+)))
    // };

    // ($self:expr, $msg:expr) => {
    //     Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, $msg))
    // };

    ($self:expr, $msg:expr) => {
        Err(format!("[ERROR_{MSG}; (col:{}, row:{})] {}", $self.pos.1, $self.pos.1, $msg))
    };
    ($self:expr, $fmt:expr, $($arg:tt)+) => {
        Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}",  $self.pos.1, $self.pos.0, format!($fmt, $($arg)+)))
    };
}

// #[macro_export]
// macro_rules! debug_pos {
//     ($self:expr, $msg:expr) => {
//         if LOG_DEBUG_INFO {
//             println!("[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, $msg)
//         }
//     };
//     ($self:expr, $fmt:expr, $($arg:tt)+) => {
//         if LOG_DEBUG_INFO {
//             println!("[DEBUG_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, format!($fmt, $($arg)+))
//         }
//     };
// }

// macro_rules! err_pos {
//     ($self:expr, $msg:expr) => {
//         Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}", $self.pos.1, $self.pos.0, $msg))
//     };
//     ($self:expr, $fmt:expr, $($arg:tt)+) => {
//         Err(format!("[ERROR_{MSG};(col:{}, row:{})] {}",  $self.pos.1, $self.pos.0, format!($fmt, $($arg)+)))
//     };
// }

// #[macro_export]
// macro_rules! errMsg {
//     // Match when there are format string and additional arguments
//     ($instance:expr, $fmt:expr, $($arg:tt)*) => {
//         format!(concat!("{ERR_MSG} ", $fmt, " at position {}"), $($arg)*, $instance.pos)
//     };
//     // Match when there is only a single string literal
//     ($instance:expr, $msg:expr) => {
//         format!("[DEBUG_{MSG}] {} at position {:?}", $msg, $instance.pos)
//     };
// }

// #[macro_export]
// macro_rules! debug_pos {
//     ($msg:expr, $pos:expr) => {{
//         if LOG_DEBUG_INFO {
//             println!("[DEBUG_{MSG};{:?}] {}", $pos, $msg)
//         }
//     }};
//     ($fmt:expr, $($arg:tt)+) => {{
//         if LOG_DEBUG_INFO {
//             println!("[DEBUG_{MSG}] {}", format!($fmt, $($arg)+))
//         }
//     }};
// }
