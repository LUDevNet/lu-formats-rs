include!(concat!(env!("OUT_DIR"), "/lib.rs"));

#[cfg(test)]
mod tests {
    use super::common::{parse_vector3, Vector3};
    use nom::Finish;

    #[test]
    fn test_vector3() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend(&0.5_f32.to_le_bytes());
        bytes.extend(&100_f32.to_le_bytes());
        bytes.extend(&7_f32.to_le_bytes());

        assert_eq!(
            parse_vector3(&bytes).finish().map(|x| x.1),
            Ok(Vector3 {
                x: 0.5,
                y: 100.0,
                z: 7.0
            })
        )
    }
}
