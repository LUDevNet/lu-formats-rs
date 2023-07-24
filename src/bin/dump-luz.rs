use lu_formats_rs::files::luz::parse_luz;
use nom::Finish;

fn main() {
    let path = std::env::args().nth(1).expect("USAGE: dump-luz FILE");
    let bytes = std::fs::read(path).expect("Failed to read file");
    let (_, luz) = parse_luz(&bytes).finish().expect("Failed to parse LUZ");
    println!("{}", serde_json::to_string(&luz).unwrap());
}
