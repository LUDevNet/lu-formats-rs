include!(concat!(env!("OUT_DIR"), "/lib.rs"));

#[cfg(test)]
mod tests {
    use crate::{
        common::{
            parse_quaternion, parse_u1_color, parse_u1_str, parse_u4_str, Bool, Quaternion,
            U1Color, U1Str, U1Wstr, U4Str,
        },
        files::luz::{parse_boundary_info, parse_camera_data, BoundaryInfo, CameraData},
    };

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

    #[test]
    fn test_u1_str() {
        let bytes = vec![0x03, b'A', b'B', b'C'];

        assert_eq!(
            parse_u1_str(&bytes).finish(),
            Ok((
                EMPTY,
                U1Str {
                    length: 3,
                    str: b"ABC"
                }
            ))
        )
    }

    #[test]
    fn test_u4_str() {
        let bytes = vec![0x03, 0x00, 0x00, 0x00, b'A', b'B', b'C'];

        assert_eq!(
            parse_u4_str(&bytes).finish(),
            Ok((
                EMPTY,
                U4Str {
                    length: 3,
                    str: b"ABC"
                }
            ))
        )
    }

    /// <https://docs.lu-dev.net/en/latest/file-structures/zone.html#luz-types-boundary_info>
    #[test]
    fn test_boundary_info() {
        let mut bytes = Vec::<u8>::new();
        let vec = Vector3 {
            x: 0.5,
            y: 100.0,
            z: 7.0,
        };
        // normal
        bytes.extend(&0.5_f32.to_le_bytes());
        bytes.extend(&100_f32.to_le_bytes());
        bytes.extend(&7_f32.to_le_bytes());
        // point
        bytes.extend(&0.5_f32.to_le_bytes());
        bytes.extend(&100_f32.to_le_bytes());
        bytes.extend(&7_f32.to_le_bytes());
        // dest_zone_id
        bytes.extend(&1200_u32.to_le_bytes());
        // dest_scene_id
        bytes.extend(&10_u32.to_le_bytes());
        // spawn_loc
        bytes.extend(&0.5_f32.to_le_bytes());
        bytes.extend(&100_f32.to_le_bytes());
        bytes.extend(&7_f32.to_le_bytes());

        assert_eq!(
            parse_boundary_info(&bytes).finish(),
            Ok((
                EMPTY,
                BoundaryInfo {
                    normal: vec.clone(),
                    point: vec.clone(),
                    dest_zone_id: 1200,
                    dest_scene_id: 10,
                    spawn_loc: vec.clone()
                }
            ))
        )
    }

    /// <https://docs.lu-dev.net/en/latest/file-structures/zone.html#luz-types-camera_data>
    #[test]
    fn test_camera_data() {
        let bytes = vec![0x03, b'A', 0x00, b'B', 0x00, b'C', 0x00, 0x01];
        assert_eq!(
            parse_camera_data(&bytes),
            Ok((
                EMPTY,
                CameraData {
                    next_path: U1Wstr {
                        length: 3,
                        str: &[b'A', 0x00, b'B', 0x00, b'C', 0x00]
                    },
                    rotate_player: Some(Bool { bool: 0x01 })
                }
            ))
        );
    }
}
