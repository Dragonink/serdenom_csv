//! When processing CSV goes wrong

use either::Either;
use serde::de::{Expected, Unexpected};
use std::fmt::{self, Display, Formatter};

/// Kind of errors that might occur during deserialization
#[derive(Debug, Clone)]
pub enum DeErrorKind {
	/// An unexpected token has been found
	SyntaxError(&'static str),
	/// The record structure does not correspond to the headers
	StructureError(usize, Option<usize>),
	/// A type different from the expected one has been found
	InvalidType(String),
	/// The visitor requested an operation that requires headers, but none were found
	NeedHeaders,
	/// The visitor requested to deserialize a structure while being in an "inner" record
	TooMuchDepth,
	/// The stream ended before the deserializer could finish
	UnexpectedEof,
	/// The deserializer finished before the stream ended
	UnexpectedEod,
}
impl Display for DeErrorKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::SyntaxError(exp) => write!(f, "syntax error: exptected {exp}"),
			Self::StructureError(unexp, exp) => {
				write!(
					f,
					"structure error: found {unexp} fields{}",
					exp.map(|val| format!(", expected {val}"))
						.unwrap_or_default()
				)
			}
			Self::InvalidType(exp) => write!(f, "invalid type: expected {exp}"),
			Self::NeedHeaders => f.write_str("headers are required for this operation"),
			Self::TooMuchDepth => f.write_str("cannot deserialize another structure level"),
			Self::UnexpectedEof => f.write_str("unexpected end-of-file"),
			Self::UnexpectedEod => f.write_str("unexpected end-of-deserialization"),
		}
	}
}

/// Error that might occur when processing CSV data
#[derive(Debug, Clone)]
pub enum Error {
	/// Generic Serde error
	Message(String),
	/// Error that might occur during deserialization
	De {
		/// Kind of error
		kind: DeErrorKind,
		/// Record where the error occurred
		record: Option<usize>,
		/// Field where the error occured
		field: Option<Either<String, usize>>,
	},
}
impl From<DeErrorKind> for Error {
	#[inline(always)]
	fn from(kind: DeErrorKind) -> Self {
		Self::De {
			kind,
			record: None,
			field: None,
		}
	}
}
impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::Message(msg) => f.write_str(msg),
			Self::De {
				kind,
				record,
				field,
			} => write!(
				f,
				"{kind}{}",
				record
					.map(|val| format!(
						" (at record #{val}{})",
						field
							.as_ref()
							.map(|val| format!(", field {val}"))
							.unwrap_or_default()
					))
					.unwrap_or_default()
			),
		}
	}
}
impl std::error::Error for Error {}
impl serde::de::Error for Error {
	#[inline]
	fn custom<T: Display>(msg: T) -> Self {
		Self::Message(msg.to_string())
	}

	#[inline]
	fn invalid_type(_unexp: Unexpected<'_>, exp: &dyn Expected) -> Self {
		Self::De {
			kind: DeErrorKind::InvalidType(exp.to_string()),
			record: None,
			field: None,
		}
	}

	#[inline]
	fn invalid_length(len: usize, _exp: &dyn Expected) -> Self {
		Self::De {
			kind: DeErrorKind::StructureError(len, None),
			record: None,
			field: None,
		}
	}
}

#[allow(missing_docs)]
pub type Result<T> = std::result::Result<T, Error>;
