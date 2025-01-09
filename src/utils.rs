static mut LOGGER: Logger = Logger {
    log_prefixes: ["LEX", "PARSE", "SEMANTIC", "CODEGEN"],
    do_logging: [false, false, false, false],
    current_prefix: LogPrefix::Lexical,
    file_name: String::new(),
    file_pos: (0, 0),
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogPrefix {
    Lexical,
    Parse,
    Semantic,
    CodeGen,
}

struct Logger {
    log_prefixes: [&'static str; 4],
    do_logging: [bool; 4],
    current_prefix: LogPrefix,
    file_name: String,
    file_pos: (u32, u32),
}

pub fn add_pos(delta: (u32, u32)) {
    unsafe {
        let pos = LOGGER.file_pos;
        LOGGER.file_pos.0 += delta.0;
        LOGGER.file_pos.1 += delta.1;

        // if do_log() {
        //     println!(
        //         "[INFO] pos update (col: {}, row: {}) => (col: {}, row: {})\n",
        //         pos.1, pos.0, LOGGER.file_pos.1, LOGGER.file_pos.0
        //     );
        // }
    }
}

pub fn sub_pos(delta: (u32, u32)) {
    unsafe {
        let pos = LOGGER.file_pos;
        LOGGER.file_pos.0 -= delta.0;
        LOGGER.file_pos.1 -= delta.1;

        // if do_log() {
        //     println!(
        //         "[INFO] pos update (col: {}, row: {}) => (col: {}, row: {})\n",
        //         pos.1, pos.0, LOGGER.file_pos.1, LOGGER.file_pos.0
        //     );
        // }
    }
}

pub fn set_pos(new_pos: (u32, u32)) {
    unsafe {
        // if do_log() {
        //     println!(
        //         "[INFO] Setting position to: (col: {}, row: {})\n",
        //         new_pos.1, new_pos.0
        //     );
        // }
        LOGGER.file_pos = new_pos;
    }
}

pub fn set_prefix(new_prefix: LogPrefix) {
    unsafe {
        LOGGER.current_prefix = new_prefix;
        set_pos((0, 0));
        if do_log() {
            match new_prefix {
                LogPrefix::Lexical => {
                    println!(
                        "\n{}\n",
                        text_to_ascii_art::to_art(">Lexical<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::Parse => {
                    println!(
                        "\n{}\n",
                        text_to_ascii_art::to_art(">Parse<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::Semantic => {
                    println!(
                        "\n{}\n",
                        text_to_ascii_art::to_art(">Semantic<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
                LogPrefix::CodeGen => {
                    println!(
                        "\n{}\n",
                        text_to_ascii_art::to_art(">CodeGen<".to_string(), "standard", 8, 0, 0)
                            .unwrap()
                    )
                }
            }
        }
    }
}

pub fn get_prefix() -> &'static str {
    unsafe { LOGGER.log_prefixes[LOGGER.current_prefix as usize] }
}

pub fn get_pos() -> (u32, u32) {
    unsafe { LOGGER.file_pos }
}

pub fn do_log() -> bool {
    unsafe { LOGGER.do_logging[LOGGER.current_prefix as usize] }
}

#[macro_export]
macro_rules! debug {
    ($msg:expr) => {
        if crate::utils::do_log() {
            println!("[DBG_{} | (col: {}, row: {})] {}",
                crate::utils::get_prefix(),
                crate::utils::get_pos().1 + 1,
                crate::utils::get_pos().0 + 1,
                format!($msg)
            )
        }
    };
    ($fmt:expr, $($arg:tt)+) => {
        if crate::utils::do_log() {
            println!(
                "[DBG_{} | (col: {}, row: {})] {}",
                crate::utils::get_prefix(),
                crate::utils::get_pos().1 + 1,
                crate::utils::get_pos().0 + 1,
                format!($fmt, $($arg)+)
            )
        }
    };
}

#[macro_export]
macro_rules! err {
    ($msg:expr) => {
        Err(anyhow::anyhow!("[ERR_{} | (col: {}, row: {})] {}\n",
            crate::utils::get_prefix(),
            crate::utils::get_pos().1 + 1,
            crate::utils::get_pos().0 + 1,
            format!($msg),
        ))
    };
    ($fmt:expr, $($arg:tt)+) => {
        Err(anyhow::anyhow!("[ERR_{} | (col: {}, row: {})] {}\n",
            crate::utils::get_prefix(),
            crate::utils::get_pos().1 + 1,
            crate::utils::get_pos().0 + 1,
            format!($fmt, $($arg)+)
        ))
    };
}
