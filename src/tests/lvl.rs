use nom::Finish;

use crate::files::lvl::{parse_fib_chunk, FibChunk};

const EMPTY: &[u8] = &[];

#[test]
fn test_fib_chunk() {
    let buf = b"CHNK\xe8\x03\0\0\x06\0\x08\0\x20\0\0\0\x30\0\0\0";
    assert_eq!(
        parse_fib_chunk(buf).finish().ok(),
        Some((
            EMPTY,
            FibChunk {
                header: (),
                r#type: (),
                header_version: 6,
                data_version: 8,
                size: 32,
                data_offset: 48
            }
        ))
    )
}
