use crate::{
    common::{Bool, QuaternionWxyz, U1Wstr},
    files::luz::{
        parse_boundary_info, parse_camera_data, parse_camera_waypoint_data, BoundaryInfo,
        CameraData, CameraWaypointData,
    },
};

use crate::common::Vector3;
use nom::Finish;

const EMPTY: &[u8] = &[];

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
        parse_camera_data(14)(&bytes),
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
    assert_eq!(
        parse_camera_data(13)(&bytes),
        Ok((
            &[0x01][..],
            CameraData {
                next_path: U1Wstr {
                    length: 3,
                    str: &[b'A', 0x00, b'B', 0x00, b'C', 0x00]
                },
                rotate_player: None
            }
        ))
    );
}

#[test]
fn test_camera_waypoint_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend(&33.3_f32.to_le_bytes());
    bytes.extend(&0.5_f32.to_le_bytes());
    bytes.extend(&100_f32.to_le_bytes());
    bytes.extend(&7_f32.to_le_bytes());

    bytes.extend(&2300_f32.to_le_bytes());
    bytes.extend(&80.2_f32.to_le_bytes());
    bytes.extend(&999_f32.to_le_bytes());
    bytes.extend(&(-300.0_f32).to_le_bytes());
    bytes.extend(&66_f32.to_le_bytes());

    assert_eq!(
        parse_camera_waypoint_data(&bytes),
        Ok((
            EMPTY,
            CameraWaypointData {
                rotation: QuaternionWxyz {
                    x: 0.5,
                    y: 100.0,
                    z: 7.0,
                    w: 33.3,
                },
                time: 2300.0,
                fov: 80.2,
                tension: 999.0,
                continuity: -300.0,
                bias: 66.0
            }
        ))
    )
}
