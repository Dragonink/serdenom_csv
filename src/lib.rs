//! Serde library for CSV (with nested structures) parsing with Nom
//!
//! # Safety
//! The `forbid(unsafe_code)` lint is enforced at crate level.
#![forbid(unsafe_code)]
#![deny(unused_must_use)]
#![warn(missing_docs)]

pub mod de;
pub mod error;

pub use de::{from_str, from_str_each};
pub use error::{Error, Result};
