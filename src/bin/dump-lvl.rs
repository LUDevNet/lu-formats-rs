use lu_formats_rs::files::lvl::{
    parse_chunk, parse_editor_settings, parse_environment_data, parse_fib_data,
    parse_lighting_info, parse_lvl, parse_object_data, parse_particle_data, parse_skydome_info,
    ChunkType, EditorSettings, FibData, LightingInfo, ObjectInfo, Particle, SkydomeInfo,
};
use nom::Finish;
use serde::Serialize;

#[derive(Serialize)]
pub struct Environment<'a> {
    lighting_info: LightingInfo,
    skydome_info: SkydomeInfo<'a>,
    editor_settings: EditorSettings,
}

#[derive(Serialize)]
pub struct Level<'a> {
    version: u32,
    revision: u32,
    environment: Option<Environment<'a>>,
    objects: Option<Vec<ObjectInfo<'a>>>,
    particles: Option<Vec<Particle<'a>>>,
}

fn parse_environment<'a>(bytes: &'a [u8], fib_data: &FibData) -> Option<Environment<'a>> {
    if fib_data.ofs_environment_chunk == 0 {
        return None;
    }
    let env_chunk_bytes = &bytes[fib_data.ofs_environment_chunk as usize..];
    let (_, env_chunk) = parse_chunk(env_chunk_bytes)
        .finish()
        .expect("Failed to parse environment_chunk");
    assert_eq!(env_chunk.r#type, ChunkType::Environment);

    let env_bytes = &bytes[env_chunk.data_offset as usize..][..(env_chunk.size - 32) as usize];
    let (_, env) = parse_environment_data(env_bytes)
        .finish()
        .expect("Failed to parse environment_data");

    let lighting_bytes = &bytes[env.ofs_lighting as usize..];
    let (_, lighting_info) = parse_lighting_info(fib_data.version)(lighting_bytes)
        .finish()
        .expect("Failed to parse lighting_info");

    let skydome_bytes = &bytes[env.ofs_skydome as usize..];
    let (_, skydome_info) = parse_skydome_info(fib_data.version)(skydome_bytes)
        .finish()
        .expect("Failed to parse skydome_info");

    let editor_settings_bytes = &bytes[env.ofs_editor_settings as usize..];
    let (_, editor_settings) = parse_editor_settings(editor_settings_bytes)
        .finish()
        .expect("Failed to parse editor_settings");

    Some(Environment {
        lighting_info,
        skydome_info,
        editor_settings,
    })
}

fn parse_objects<'a>(bytes: &'a [u8], fib_data: &FibData) -> Option<Vec<ObjectInfo<'a>>> {
    if fib_data.ofs_object_chunk == 0 {
        return None;
    }
    let object_chunk_bytes = &bytes[fib_data.ofs_object_chunk as usize..];
    let (_, object_chunk) = parse_chunk(object_chunk_bytes)
        .finish()
        .expect("Failed to parse object_chunk");
    assert_eq!(object_chunk.r#type, ChunkType::Object);
    let object_bytes =
        &bytes[object_chunk.data_offset as usize..][..(object_chunk.size - 32) as usize];
    let (_, objects) = parse_object_data(fib_data.version)(object_bytes)
        .finish()
        .expect("Failed to parse object_data");
    Some(objects.objects)
}

fn parse_particles<'a>(bytes: &'a [u8], fib_data: &FibData) -> Option<Vec<Particle<'a>>> {
    if fib_data.ofs_particle_chunk == 0 {
        return None;
    }
    let chunk_bytes = &bytes[fib_data.ofs_particle_chunk as usize..];
    let (_, chunk) = parse_chunk(chunk_bytes)
        .finish()
        .expect("Failed to parse particle_chunk");
    assert_eq!(chunk.r#type, ChunkType::Particle);
    let particle_bytes = &bytes[chunk.data_offset as usize..][..(chunk.size - 32) as usize];
    let (_, particle_data) = parse_particle_data(fib_data.version)(particle_bytes)
        .finish()
        .expect("Failed to parse object_data");
    Some(particle_data.particles)
}

fn parse_level(bytes: &[u8]) -> Level<'_> {
    let (_, lvl) = parse_lvl(&bytes).finish().expect("Failed to parse lvl");

    let fib_bytes =
        &bytes[lvl.fib_chunk.data_offset as usize..][..(lvl.fib_chunk.size - 32) as usize];
    let (_, fib_data) = parse_fib_data(fib_bytes)
        .finish()
        .expect("Failed to parse fib_data");

    let environment = parse_environment(&bytes, &fib_data);
    let objects = parse_objects(&bytes, &fib_data);
    let particles = parse_particles(&bytes, &fib_data);
    Level {
        version: fib_data.version,
        revision: fib_data.revision,
        environment,
        objects,
        particles,
    }
}

fn main() {
    let path = std::env::args().nth(1).expect("USAGE: dump-lvl FILE");
    let bytes = std::fs::read(path).expect("Failed to read file");
    let level = parse_level(&bytes);
    println!("{}", serde_json::to_string(&level).unwrap());
}
