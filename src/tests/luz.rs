use crate::{
    common::{Bool, Lot, QuaternionWxyz, U1Str, U1Wstr, U4Wstr},
    files::luz::{
        parse_boundary_info, parse_camera_data, parse_camera_waypoint_data, parse_lnv,
        parse_lnv_entry, parse_path, parse_platform_data, parse_platform_waypoint_data,
        parse_property_data, parse_racing_waypoint_data, parse_rail_waypoint_data,
        parse_spawner_data, parse_spawner_waypoint_data, parse_transition_info,
        parse_transition_point, parse_waypoint, BoundaryInfo, CameraData, CameraWaypointData, Lnv,
        LnvEntry, NpcWaypointData, Path, PlatformData, PlatformWaypointData, PropertyData,
        RacingWaypointData, RailWaypointData, SpawnerData, SpawnerWaypointData, TransitionInfo,
        TransitionPoint, Waypoint, WaypointDataVariants,
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

#[test]
fn test_racing_waypoint_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend(&0f32.to_le_bytes());
    bytes.extend(&1f32.to_le_bytes());
    bytes.extend(&1f32.to_le_bytes());
    bytes.extend(&1f32.to_le_bytes());
    bytes.extend(&[0x01, 0x00]);
    bytes.extend(&0.4_f32.to_le_bytes());
    bytes.extend(&0.5_f32.to_le_bytes());
    bytes.extend(&0.6_f32.to_le_bytes());

    assert_eq!(
        parse_racing_waypoint_data(&bytes),
        Ok((
            EMPTY,
            RacingWaypointData {
                rotation: QuaternionWxyz {
                    w: 0.0,
                    x: 1.0,
                    y: 1.0,
                    z: 1.0
                },
                is_reset_node: Bool { bool: 0x01 },
                is_non_horizontal_camera: Bool { bool: 0x00 },
                plane_width: 0.4,
                plane_height: 0.5,
                shortest_distance_to_end: 0.6
            }
        ))
    )
}

#[test]
fn test_rail_waypoint() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&25f32.to_le_bytes());
    bytes.extend_from_slice(&26f32.to_le_bytes());
    bytes.extend_from_slice(&27f32.to_le_bytes());

    bytes.extend_from_slice(&1.0f32.to_le_bytes());
    bytes.extend_from_slice(&1.1f32.to_le_bytes());
    bytes.extend_from_slice(&1.2f32.to_le_bytes());
    bytes.extend_from_slice(&1.3f32.to_le_bytes());
    bytes.extend(&0u32.to_le_bytes());

    assert_eq!(
        parse_waypoint(parse_rail_waypoint_data(16))(&bytes),
        Ok((
            EMPTY,
            Waypoint::<RailWaypointData> {
                position: Vector3 {
                    x: 25.0,
                    y: 26.0,
                    z: 27.0
                },
                data: RailWaypointData {
                    rotation: QuaternionWxyz {
                        w: 1.0,
                        x: 1.1,
                        y: 1.2,
                        z: 1.3
                    },
                    speed: None,
                    config: Lnv {
                        num_entries: 0,
                        entries: vec![]
                    }
                }
            }
        ))
    );
}

#[test]
fn test_rail_waypoint_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&1.0f32.to_le_bytes());
    bytes.extend_from_slice(&1.1f32.to_le_bytes());
    bytes.extend_from_slice(&1.2f32.to_le_bytes());
    bytes.extend_from_slice(&1.3f32.to_le_bytes());
    bytes.extend(&0u32.to_le_bytes());

    assert_eq!(
        parse_rail_waypoint_data(16)(&bytes),
        Ok((
            EMPTY,
            RailWaypointData {
                rotation: QuaternionWxyz {
                    w: 1.0,
                    x: 1.1,
                    y: 1.2,
                    z: 1.3
                },
                speed: None,
                config: Lnv {
                    num_entries: 0,
                    entries: vec![]
                }
            }
        ))
    );

    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&1.0f32.to_le_bytes());
    bytes.extend_from_slice(&1.1f32.to_le_bytes());
    bytes.extend_from_slice(&1.2f32.to_le_bytes());
    bytes.extend_from_slice(&1.3f32.to_le_bytes());
    bytes.extend_from_slice(&5.0f32.to_le_bytes());
    bytes.extend(&0u32.to_le_bytes());

    assert_eq!(
        parse_rail_waypoint_data(17)(&bytes),
        Ok((
            EMPTY,
            RailWaypointData {
                rotation: QuaternionWxyz {
                    w: 1.0,
                    x: 1.1,
                    y: 1.2,
                    z: 1.3
                },
                speed: Some(5.0),
                config: Lnv {
                    num_entries: 0,
                    entries: vec![]
                }
            }
        ))
    );
}

#[test]
fn test_spawner_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&1232u32.to_le_bytes());
    bytes.extend_from_slice(&3600u32.to_le_bytes());
    bytes.extend_from_slice(&5u32.to_le_bytes());
    bytes.extend_from_slice(&3u32.to_le_bytes());
    bytes.extend_from_slice(&700000000u64.to_le_bytes());

    assert_eq!(
        parse_spawner_data(8)(&bytes),
        Ok((
            EMPTY,
            SpawnerData {
                spawned_lot: Lot { lot: 1232 },
                respawn_time: 3600,
                max_to_spawn: 5,
                num_to_maintain: 3,
                object_id: crate::common::ObjectId {
                    object_id: 700000000
                },
                activate_on_load: None
            }
        ))
    );

    bytes.push(0x00);

    assert_eq!(
        parse_spawner_data(9)(&bytes),
        Ok((
            EMPTY,
            SpawnerData {
                spawned_lot: Lot { lot: 1232 },
                respawn_time: 3600,
                max_to_spawn: 5,
                num_to_maintain: 3,
                object_id: crate::common::ObjectId {
                    object_id: 700000000
                },
                activate_on_load: Some(Bool { bool: 0x00 })
            }
        ))
    );
}

#[test]
fn test_spawner_waypoint_data() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&1.0f32.to_le_bytes());
    bytes.extend_from_slice(&2.0f32.to_le_bytes());
    bytes.extend_from_slice(&4.0f32.to_le_bytes());
    bytes.extend_from_slice(&8.0f32.to_le_bytes());
    bytes.extend_from_slice(&0u32.to_le_bytes());

    assert_eq!(
        parse_spawner_waypoint_data(&bytes),
        Ok((
            EMPTY,
            SpawnerWaypointData {
                rotation: QuaternionWxyz {
                    w: 1.0,
                    x: 2.0,
                    y: 4.0,
                    z: 8.0
                },
                config: Lnv {
                    num_entries: 0,
                    entries: vec![]
                }
            }
        ))
    );
}

#[test]
fn test_transition_point() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&10u32.to_le_bytes());
    bytes.extend_from_slice(&1u32.to_le_bytes());
    bytes.extend_from_slice(&1.0f32.to_le_bytes());
    bytes.extend_from_slice(&2.0f32.to_le_bytes());
    bytes.extend_from_slice(&4.0f32.to_le_bytes());

    assert_eq!(
        parse_transition_point(&bytes),
        Ok((
            EMPTY,
            TransitionPoint {
                scene_id: 10,
                layer_id: 1,
                transition_point: Vector3 {
                    x: 1.0,
                    y: 2.0,
                    z: 4.0
                }
            }
        ))
    );
}

#[test]
fn test_transition_info() {
    let mut bytes = Vec::<u8>::new();
    bytes.push(0x00);
    bytes.extend_from_slice(&0.5f32.to_le_bytes());

    bytes.extend_from_slice(&10u32.to_le_bytes());
    bytes.extend_from_slice(&1u32.to_le_bytes());
    bytes.extend_from_slice(&1f32.to_le_bytes());
    bytes.extend_from_slice(&2f32.to_le_bytes());
    bytes.extend_from_slice(&4f32.to_le_bytes());

    bytes.extend_from_slice(&10u32.to_le_bytes());
    bytes.extend_from_slice(&1u32.to_le_bytes());
    bytes.extend_from_slice(&100f32.to_le_bytes());
    bytes.extend_from_slice(&99f32.to_le_bytes());
    bytes.extend_from_slice(&98f32.to_le_bytes());

    let p1 = TransitionPoint {
        scene_id: 10,
        layer_id: 1,
        transition_point: Vector3 {
            x: 1.0,
            y: 2.0,
            z: 4.0,
        },
    };
    let p2 = TransitionPoint {
        scene_id: 10,
        layer_id: 1,
        transition_point: Vector3 {
            x: 100.0,
            y: 99.0,
            z: 98.0,
        },
    };

    assert_eq!(
        parse_transition_info(33)(&bytes),
        Ok((
            EMPTY,
            TransitionInfo {
                unknown1: Some(U1Str {
                    length: 0,
                    str: EMPTY
                }),
                unknown2: Some(0.5),
                transition_points: vec![p1.clone(), p2.clone()]
            }
        ))
    );

    for _ in 0..3 {
        bytes.extend_from_slice(&10u32.to_le_bytes());
        bytes.extend_from_slice(&1u32.to_le_bytes());
        bytes.extend_from_slice(&1f32.to_le_bytes());
        bytes.extend_from_slice(&2f32.to_le_bytes());
        bytes.extend_from_slice(&4f32.to_le_bytes());
    }

    assert_eq!(
        parse_transition_info(34)(&bytes),
        Ok((
            EMPTY,
            TransitionInfo {
                unknown1: Some(U1Str {
                    length: 0,
                    str: EMPTY
                }),
                unknown2: Some(0.5),
                transition_points: vec![p1.clone(), p2.clone(), p1.clone(), p1.clone(), p1.clone()]
            }
        ))
    );

    bytes.drain((bytes.len() - 3 * 20)..);

    assert_eq!(
        parse_transition_info(39)(&bytes),
        Ok((
            EMPTY,
            TransitionInfo {
                unknown1: Some(U1Str {
                    length: 0,
                    str: EMPTY
                }),
                unknown2: Some(0.5),
                transition_points: vec![p1.clone(), p2.clone()]
            }
        ))
    );

    assert_eq!(
        parse_transition_info(40)(&bytes[5..]),
        Ok((
            EMPTY,
            TransitionInfo {
                unknown1: None,
                unknown2: None,
                transition_points: vec![p1.clone(), p2.clone()]
            }
        ))
    );
}

#[test]
fn test_path() {
    let mut bytes = Vec::<u8>::new();
    bytes.extend_from_slice(&1u32.to_le_bytes());
    bytes.extend_from_slice(b"\x04N\0a\0m\0e\0");
    bytes.extend_from_slice(b"\x02A\0Z\0");
    bytes.extend_from_slice(&0u32.to_le_bytes()); // NPC
    bytes.extend_from_slice(&100u32.to_le_bytes()); // flags
    bytes.extend_from_slice(&2u32.to_le_bytes()); // path behavior loop
    bytes.extend_from_slice(&1u32.to_le_bytes()); // 1 waypoint

    bytes.extend_from_slice(&10f32.to_le_bytes()); // x
    bytes.extend_from_slice(&11f32.to_le_bytes()); // y
    bytes.extend_from_slice(&12f32.to_le_bytes()); // z
    bytes.extend_from_slice(&0u32.to_le_bytes()); // 0 config entries (npc)

    assert_eq!(
        parse_path(&bytes),
        Ok((
            EMPTY,
            Path {
                version: 1,
                name: U1Wstr {
                    length: 4,
                    str: b"N\0a\0m\0e\0"
                },
                type_name: Some(U1Wstr {
                    length: 2,
                    str: b"A\0Z\0"
                }),
                r#type: 0,
                flags: 100,
                behavior: 2,
                data: crate::files::luz::PathDataVariants::_Other,
                num_waypoints: 1,
                waypoints: vec![Waypoint::<WaypointDataVariants> {
                    position: Vector3 {
                        x: 10.0,
                        y: 11.0,
                        z: 12.0
                    },
                    data: WaypointDataVariants::Npc(NpcWaypointData {
                        config: Lnv {
                            num_entries: 0,
                            entries: vec![]
                        }
                    })
                }]
            }
        ))
    );
}
