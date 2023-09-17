mod data;
mod parse;

fn main() {
    let path = std::env::args().nth(1).expect("USAGE: dump-lvl FILE");
    let bytes = std::fs::read(path).expect("Failed to read file");
    let level = parse::parse_level(&bytes);
    println!("{}", serde_json::to_string(&level).unwrap());
}
