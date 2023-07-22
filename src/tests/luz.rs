use crate::{
    common::{Bool, QuaternionWxyz, U1Wstr, U4Wstr},
    files::luz::{
        parse_boundary_info, parse_camera_data, parse_camera_waypoint_data, parse_lnv,
        parse_lnv_entry, parse_platform_data, parse_platform_waypoint_data, parse_property_data,
        BoundaryInfo, CameraData, CameraWaypointData, Lnv, LnvEntry, PlatformData,
        PlatformWaypointData, PropertyData,
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

#[test]
fn test_lnv_entry() {
    let bytes = vec![
        0x03, b'A', 0, b'B', 0, b'C', 0, 0x04, b'D', 0, b'E', 0, b'F', 0, b'G', 0,
    ];
    assert_eq!(
        parse_lnv_entry(&bytes),
        Ok((
            EMPTY,
            LnvEntry {
                name: U1Wstr {
                    length: 3,
                    str: b"A\0B\0C\0",
                },
                type_value: U1Wstr {
                    length: 4,
                    str: b"D\0E\0F\0G\0",
                }
            }
        ))
    )
}

#[test]
fn test_lnv() {
    let bytes = vec![
        0x02, 0, 0, 0, //
        0x03, b'A', 0, b'B', 0, b'C', 0, 0x04, b'D', 0, b'E', 0, b'F', 0, b'G', 0, //
        0x03, b'A', 0, b'B', 0, b'C', 0, 0x04, b'D', 0, b'E', 0, b'F', 0, b'G', 0,
    ];
    assert_eq!(
        parse_lnv(&bytes),
        Ok((
            EMPTY,
            Lnv {
                num_entries: 2,
                entries: vec![
                    LnvEntry {
                        name: U1Wstr {
                            length: 3,
                            str: b"A\0B\0C\0",
                        },
                        type_value: U1Wstr {
                            length: 4,
                            str: b"D\0E\0F\0G\0",
                        }
                    },
                    LnvEntry {
                        name: U1Wstr {
                            length: 3,
                            str: b"A\0B\0C\0",
                        },
                        type_value: U1Wstr {
                            length: 4,
                            str: b"D\0E\0F\0G\0",
                        }
                    }
                ]
            }
        ))
    )
}

#[test]
fn test_platform_data() {
    let bytes: Vec<u8> = vec![];
    assert_eq!(
        parse_platform_data(12)(&bytes),
        Ok((
            EMPTY,
            PlatformData {
                traveling_audio_guid: None,
                time_based_movement: None
            }
        ))
    );

    let bytes: Vec<u8> = vec![0x02, b'A', 0, b'C', 0];
    assert_eq!(
        parse_platform_data(13)(&bytes),
        Ok((
            EMPTY,
            PlatformData {
                traveling_audio_guid: Some(U1Wstr {
                    length: 2,
                    str: b"A\0C\0"
                }),
                time_based_movement: None
            }
        ))
    );

    let bytes: Vec<u8> = vec![0x01];
    assert_eq!(
        parse_platform_data(18)(&bytes),
        Ok((
            EMPTY,
            PlatformData {
                traveling_audio_guid: None,
                time_based_movement: Some(Bool { bool: 0x01 })
            }
        ))
    );
}

#[test]
fn test_platform_waypoint_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend(&33.3_f32.to_le_bytes());
    bytes.extend(&0.5_f32.to_le_bytes());
    bytes.extend(&100_f32.to_le_bytes());
    bytes.extend(&7_f32.to_le_bytes());

    bytes.push(0x01);
    bytes.extend(&0.25_f32.to_le_bytes());
    bytes.extend(&0.75_f32.to_le_bytes());

    assert_eq!(
        parse_platform_waypoint_data(12)(&bytes),
        Ok((
            EMPTY,
            PlatformWaypointData {
                rotation: QuaternionWxyz {
                    w: 33.3,
                    x: 0.5,
                    y: 100.0,
                    z: 7.0
                },
                lock_player: Bool { bool: 0x01 },
                speed: 0.25,
                wait: 0.75,
                depart_audio_guid: None,
                arrive_audio_guid: None
            }
        ))
    );

    bytes.extend(&[0x03, b'A', 0, b'B', 0, b'C', 0]);
    bytes.extend(&[0x03, b'X', 0, b'Y', 0, b'Z', 0]);

    assert_eq!(
        parse_platform_waypoint_data(13)(&bytes),
        Ok((
            EMPTY,
            PlatformWaypointData {
                rotation: QuaternionWxyz {
                    w: 33.3,
                    x: 0.5,
                    y: 100.0,
                    z: 7.0
                },
                lock_player: Bool { bool: 0x01 },
                speed: 0.25,
                wait: 0.75,
                depart_audio_guid: Some(U1Wstr {
                    length: 3,
                    str: b"A\0B\0C\0"
                }),
                arrive_audio_guid: Some(U1Wstr {
                    length: 3,
                    str: b"X\0Y\0Z\0"
                })
            }
        ))
    );
}

#[test]
fn test_property_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend(1u32.to_le_bytes());
    bytes.extend(50u32.to_le_bytes());
    bytes.extend(100u32.to_le_bytes());
    bytes.extend(1200u64.to_le_bytes());

    assert_eq!(
        parse_property_data(4)(&bytes),
        Ok((
            EMPTY,
            PropertyData {
                property_path_type: 1,
                price: 50,
                time: 100,
                associated_zone: 1200,
                name: None,
                description: None,
                property_type: None,
                clone_limit: None,
                reputation_multiplier: None,
                period_type: None,
                achievement_required: None,
                zone_position: None,
                max_build_height: None
            }
        ))
    );

    bytes.extend(b"\x04N\0a\0m\0e\0");
    bytes.extend(b"\x05\0\0\0D\0e\0s\0c\0r\0");

    assert_eq!(
        parse_property_data(5)(&bytes),
        Ok((
            EMPTY,
            PropertyData {
                property_path_type: 1,
                price: 50,
                time: 100,
                associated_zone: 1200,
                name: Some(U1Wstr {
                    length: 4,
                    str: b"N\0a\0m\0e\0"
                }),
                description: Some(U4Wstr {
                    length: 5,
                    str: b"D\0e\0s\0c\0r\0"
                }),
                property_type: None,
                clone_limit: None,
                reputation_multiplier: None,
                period_type: None,
                achievement_required: None,
                zone_position: None,
                max_build_height: None
            }
        ))
    );

    bytes.extend(&2u32.to_le_bytes());
    assert_eq!(
        parse_property_data(6)(&bytes),
        Ok((
            EMPTY,
            PropertyData {
                property_path_type: 1,
                price: 50,
                time: 100,
                associated_zone: 1200,
                name: Some(U1Wstr {
                    length: 4,
                    str: b"N\0a\0m\0e\0"
                }),
                description: Some(U4Wstr {
                    length: 5,
                    str: b"D\0e\0s\0c\0r\0"
                }),
                property_type: Some(2),
                clone_limit: None,
                reputation_multiplier: None,
                period_type: None,
                achievement_required: None,
                zone_position: None,
                max_build_height: None
            }
        ))
    );

    bytes.extend(&20000u32.to_le_bytes());
    bytes.extend(&5f32.to_le_bytes());
    bytes.extend(&2u32.to_le_bytes());
    assert_eq!(
        parse_property_data(7)(&bytes),
        Ok((
            EMPTY,
            PropertyData {
                property_path_type: 1,
                price: 50,
                time: 100,
                associated_zone: 1200,
                name: Some(U1Wstr {
                    length: 4,
                    str: b"N\0a\0m\0e\0"
                }),
                description: Some(U4Wstr {
                    length: 5,
                    str: b"D\0e\0s\0c\0r\0"
                }),
                property_type: Some(2),
                clone_limit: Some(20000),
                reputation_multiplier: Some(5.0),
                period_type: Some(2),
                achievement_required: None,
                zone_position: None,
                max_build_height: None
            }
        ))
    );

    bytes.extend(&4444u32.to_le_bytes());

    bytes.extend(&20f32.to_le_bytes());
    bytes.extend(&20f32.to_le_bytes());
    bytes.extend(&20f32.to_le_bytes());

    bytes.extend(&150f32.to_le_bytes());

    assert_eq!(
        parse_property_data(8)(&bytes),
        Ok((
            EMPTY,
            PropertyData {
                property_path_type: 1,
                price: 50,
                time: 100,
                associated_zone: 1200,
                name: Some(U1Wstr {
                    length: 4,
                    str: b"N\0a\0m\0e\0"
                }),
                description: Some(U4Wstr {
                    length: 5,
                    str: b"D\0e\0s\0c\0r\0"
                }),
                property_type: Some(2),
                clone_limit: Some(20000),
                reputation_multiplier: Some(5.0),
                period_type: Some(2),
                achievement_required: Some(4444),
                zone_position: Some(Vector3 {
                    x: 20.0,
                    y: 20.0,
                    z: 20.0
                }),
                max_build_height: Some(150.0)
            }
        ))
    );
}
