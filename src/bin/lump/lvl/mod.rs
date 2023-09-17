use self::data::Level;

mod data;
mod parse;

pub fn dump_lvl(path: &str) {
    let bytes = std::fs::read(path).expect("Failed to read file");
    let level = parse::parse_level(&bytes);
    println!("{}", serde_json::to_string(&level).unwrap());
}

pub fn rebuild_lvl(_path: &str) {
    let level: Level<'static> =
        serde_json::from_reader(std::io::stdin()).expect("Failed to parse JSON");
    println!("{:?}", level);
}
