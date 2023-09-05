use lu_formats_rs::files::lvl::parse_lvl;
use nom::Finish;

fn main() {
    let path = std::env::args().nth(1).expect("USAGE: dump-lvl FILE");
    let bytes = std::fs::read(path).expect("Failed to read file");
    let (_, lvl) = parse_lvl(&bytes).finish().expect("Failed to parse lvl");
    println!("{}", serde_json::to_string(&lvl).unwrap());
}
