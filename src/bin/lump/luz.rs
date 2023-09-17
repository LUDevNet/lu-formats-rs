use lu_formats_rs::files::luz::parse_luz;
use nom::Finish;

pub fn dump_luz(path: &str) {
    let bytes = std::fs::read(path).expect("Failed to read file");
    let (_, luz) = parse_luz(&bytes).finish().expect("Failed to parse LUZ");
    println!("{}", serde_json::to_string(&luz).unwrap());
}
