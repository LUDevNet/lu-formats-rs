include!(concat!(env!("OUT_DIR"), "/lib.rs"));

#[cfg(test)]
mod tests {
    use crate::common::{parse_quaternion, parse_u1_color, Quaternion, U1Color};

    use super::common::{parse_vector3, Vector3};
    use nom::Finish;

    const EMPTY: &[u8] = &[];

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

    #[test]
    fn test_quaternion() {
        let mut bytes = Vec::<u8>::new();
        bytes.extend(&0.5_f32.to_le_bytes());
        bytes.extend(&100_f32.to_le_bytes());
        bytes.extend(&7_f32.to_le_bytes());
        bytes.extend(&33.3_f32.to_le_bytes());

        assert_eq!(
            parse_quaternion(&bytes).finish(),
            Ok((
                EMPTY,
                Quaternion {
                    x: 0.5,
                    y: 100.0,
                    z: 7.0,
                    w: 33.3,
                }
            ))
        )
    }

    #[test]
    fn test_u1color() {
        let bytes = vec![0xFF, 0x50, 0x71];

        assert_eq!(
            parse_u1_color(&bytes).finish(),
            Ok((
                EMPTY,
                U1Color {
                    r: 0xFF,
                    g: 0x50,
                    b: 0x71,
                }
            ))
        )
    }
}
