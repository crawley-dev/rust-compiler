use std::io::{self, BufRead};

pub fn get_file_name() -> (String, String) {
    let file_path: String = std::env::args().skip(1).take(1).collect();
    let file_name = file_path
        .split('/')
        .nth(2)
        .unwrap_or("default.txt")
        .split('.')
        .next()
        .unwrap()
        .to_owned();
    if file_path.is_empty() {
        panic!("[COMPILER] Invalid File Path!\n");
    }
    return (file_path, file_name);
}

pub fn get_file_contents(file_path: String) -> Vec<String> {
    let file = std::fs::File::open(file_path)
        .unwrap_or_else(|_| panic!("[COMPILER] Error opening file\n"));
    return io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap() + "\n") // string literal
        .collect::<Vec<String>>();
}
