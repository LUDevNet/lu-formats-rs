use luz::dump_luz;
use lvl::{dump_lvl, rebuild_lvl};

mod luz;
mod lvl;

fn main() {
    let mut args = std::env::args();
    let usage = "USAGE: lump [decode|encode] [luz|lvl] FILE";
    let cmd = args.nth(1).expect(usage);
    let format = args.next().expect(usage);
    let path = args.next().expect(usage);
    match (cmd.as_str(), format.as_str()) {
        ("decode", "lvl") => dump_lvl(&path),
        ("encode", "lvl") => rebuild_lvl(&path),
        ("decode", "luz") => dump_luz(&path),
        ("encode", "luz") => todo!(),
        _ => todo!("{}", usage),
    }
}
