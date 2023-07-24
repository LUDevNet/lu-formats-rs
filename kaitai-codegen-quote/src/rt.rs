use quote::quote;
use std::{io, path::Path};

pub fn codegen_rt(out_path: &Path) -> Result<(), io::Error> {
    let tokens = quote!(
        #![allow(dead_code)]

        #[cfg(feature = "serde")]
        pub fn serialize_utf16_le<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            serializer.serialize_str(
                &char::decode_utf16(value.chunks(2).map(|s| u16::from_le_bytes([s[0], s[1]])))
                    .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
                    .collect::<String>(),
            )
        }

        #[cfg(feature = "serde")]
        pub fn serialize_utf16_be<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            serializer.serialize_str(
                &char::decode_utf16(value.chunks(2).map(|s| u16::from_be_bytes([s[0], s[1]])))
                    .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
                    .collect::<String>(),
            )
        }

        #[cfg(feature = "serde")]
        pub fn serialize_utf8<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            serializer.serialize_str(&String::from_utf8_lossy(value))
        }

        #[cfg(feature = "serde")]
        pub fn serialize_ascii<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ::serde::Serializer,
        {
            // FIXME: check for non-ascii bytes?
            serializer.serialize_str(&String::from_utf8_lossy(value))
        }
    );
    super::write_file(out_path, tokens)
}
