//! Deserializing some CSV inputs
//!
//! # Deserialization methods
//! `serdenom_csv` provides two methods for CSV deserialization:
//!
//! | Return type | Default options function | Customizable options function |
//! |---|---|---|
//! | `Vec<Result<T>>` | [`from_str_each`] | [`DeserializerBuilder::deserialize_each`] |
//! | `T` | [`from_str`] | [`DeserializerBuilder::deserialize`] |
//!
//! **The difference between them is when they fail.**
//! The first method returns a `Vec<Result<T>>`, which allows it not to fail if an error occurs while deserializing some records.
//! On the contrary, the second method returns only a `T`, and will fail if an error occurs anywhere.
//!
//! ## Which method do I choose?
//! If you have a standard CSV file with several records (which is very likely),
//! you should use [`from_str_each`] or [`DeserializerBuilder::deserialize_each`].
//! If you have a non-standard CSV file with a single structure or a list of values,
//! you need [`from_str`] or [`DeserializerBuilder::deserialize`].
//!
//! With that said, you may still use the second method instead of the first one if you do not care or want a fail-fast method:
//! ```rust
//! # use serdenom_csv::*;
//! # #[derive(Debug, PartialEq, Eq, serde::Deserialize)] struct T { value: usize }
//! # const INPUT: &str = "value";
//! let data: Vec<T> = from_str(INPUT).unwrap();
//!	let data_each: Vec<T> = from_str_each(INPUT)
//!		.unwrap()
//!		.into_iter()
//!		.map(|res| res.unwrap())
//!		.collect();
//!	assert_eq!(data, data_each);
//! ```
//!
//! # Supported types
//! `serdenom_csv`'s deserializer **implements all [`serde::de::Deserializer`] methods**.
//! Which means that all types (including structs, [enums](self#Enums) and sequence-like types) are supported and can be nested.
//!
//! ## Nested types
//! CSV standard format does not allow nested types.
//! `serdenom_csv` circumvents the issue by letting the user choose [`Separators`] levels.
//!
//! The default deserializer complies with the CSV standard:
//! there is only one level, defined as `Separators('\n', ',')`.
//!
//! ### Example
//! If you need to deserialize a struct `Data` like this:
//! ```rust
//! # use serde::Deserialize;
//! #[derive(Deserialize)]
//! struct Nested<'s> {
//! 	value: usize,
//! 	text: &'s str,
//! }
//! #[derive(Deserialize)]
//! struct Data<'s> {
//! 	id: usize,
//! 	#[serde(borrow)]
//! 	nested: Vec<Nested<'s>>,
//! }
//! ```
//! ... you may write your CSV like this:
//! ```csv
//! id,nested
//! 0,86413:foo bar;6846:hello there
//! ```
//! ... such that the following code deserializes it:
//! ```rust
//! # use serdenom_csv::de::*;
//! # #[derive(Debug, PartialEq, Eq, serde::Deserialize)] struct Nested<'s> { value: usize, text: &'s str }
//! # #[derive(Debug, PartialEq, Eq, serde::Deserialize)] struct Data<'s> { id: usize, #[serde(borrow)] nested: Vec<Nested<'s>> }
//! # const INPUT: &str = "id,nested\n0,86413:foo bar;6846:hello there";
//! let deserializer = DeserializerBuilder::default()
//! 	.separators([Separators::default(), Separators(';', ':')]);
//! let data: Vec<Data> = deserializer.deserialize(INPUT).unwrap();
//!
//! assert_eq!(data[0].id, 0);
//! assert_eq!(data[0].nested[0], Nested {
//! 	value: 86413,
//! 	text: "foo bar",
//! });
//! assert_eq!(data[0].nested[1], Nested {
//! 	value: 6846,
//! 	text: "hello there",
//! });
//! ```
//!
//! In the CSV, we wrote the `nested` value as `86413:foo bar;6846:hello there`.
//! The colon acts as a field separator, while the semicolon acts as a record separator.
//!
//! Now we need to tell this to the deserializer so it can do its job properly.
//! That's what we do by calling the [`DeserializerBuilder::separators`] method with `[Separators::default(), Separators(';', ':')]`.
//! Here we keep the default level (records are separated by a newline character, fields by a comma), and add a new level with `Separators(';', ':')`
//! which describes exactly what we saw in the previous paragraph.
//!
//! ## Borrowed types
//! As you may have seen in some examples, `serdenom_csv` supports borrowed strings.
//!
//! Currently, **`serdenom_csv` does not support borrowed bytes**.
//! This means that you need to type your bytes as `Vec<u8>` and represent them as a list of numbers in CSV.
//!
//! Borrowed types allow you to borrow data from the deserializer by using zero-copy deserialization.
//! So, instead of deserializing this:
//! ```rust
//! struct T {
//! 	text: String,
//! }
//! ```
//! ... you can deserialize this:
//! ```rust
//! struct T<'s> {
//! 	text: &'s str,
//! }
//! ```
//!
//! Please see the [Serde documentation](https://serde.rs/lifetimes.html#borrowing-data-in-a-derived-impl) for more details.
//!
//! ## Enums
//! `serdenom_csv` represents enums like structs, with a prepended field which sets the enum variant.
//! This is called *internally tagged* in [Serde documentation](https://serde.rs/enum-representations.html#internally-tagged),
//! even though Serde consider enums as *externally tagged* if the `#[serde(tag)]` attribute is missing.
//!
//! ```csv
//! unit
//! newtype,42
//! tuple,true,"Hello, world!"
//! struct,69,foo bar
//! ```
//! ```rust
//! # use serde::Deserialize;
//! # use serdenom_csv::de::DeserializerBuilder;
//! #[derive(Debug, PartialEq, Eq, Deserialize)]
//! #[serde(rename_all = "lowercase")]
//! enum E<'s> {
//! 	Unit,
//! 	NewType(usize),
//! 	Tuple(bool, &'s str),
//! 	Struct { num: usize, text: &'s str },
//! }
//!
//! # const INPUT: &str = "unit\nnewtype,42\ntuple,true,\"Hello, world!\"\nstruct,69,foo bar";
//! let deserializer = DeserializerBuilder::default()
//! 	.has_headers(false);
//! let data: Vec<E> = deserializer.deserialize(INPUT).unwrap();
//! ```

use crate::error::DeErrorKind;
use nom::{branch::Alt, IResult};
use serde::{
	de::{self, DeserializeSeed, MapAccess, SeqAccess, VariantAccess, Visitor},
	forward_to_deserialize_any, serde_if_integer128, Deserialize,
};
use std::{collections::HashSet, fmt::Debug, ops::Neg, str::FromStr};

type NomError<'de> = nom::error::Error<&'de str>;
type NomErr<'de> = nom::Err<NomError<'de>>;

/// Deserialize a CSV input using the default deserializer
///
/// This function returns a generic `T` such that it can deserialize anything.
/// This means that this function will fail if an error occur anywhere during the process.
///
/// # See also
/// - If you want to customize the deserializer's options, you need [`DeserializerBuilder::deserialize`] instead.
/// - If you want to deserialize a sequence of standard CSV records, you should use [`from_str_each`] instead.
#[inline]
pub fn from_str<'de, T: Deserialize<'de>>(input: &'de str) -> crate::Result<T> {
	DeserializerBuilder::default().deserialize(input)
}

/// Deserialize each record of a CSV input independently using the default deserializer
///
/// This function returns a `Vec<Result<T>>` such that if the deserializations of some records fail,
/// the others will still be deserialized successfully.
///
/// # See also
/// - If you want to customize the deserializer's options, you need [`DeserializerBuilder::deserialize_each`] instead.
/// - If you want to deserialize some non-standard CSV structure, you should use [`from_str`] instead.
#[inline]
pub fn from_str_each<'de, T: Deserialize<'de>>(
	input: &'de str,
) -> crate::Result<Vec<crate::Result<T>>> {
	DeserializerBuilder::default().deserialize_each(input)
}

/// Define a level of nested types in CSV data
///
/// # Examples
/// Let us say that you have this struct:
/// ```rust
/// struct T<'s> {
/// 	id: usize,
/// 	firstname: &'s str,
/// 	lastname: &'s str,
/// }
/// ```
///
/// Now let us see how `T` records would be represented in CSV using different separators:
/// - `Separators('\n', ',')` ([default](Self::default))
///   ```csv
///   id,firstname,lastname
///   865,Jean,Dupont
///   785421,John,Doe
///   ```
/// - `Separators(';', ',')`
///   ```csv
///   id,firstname,lastname;865,Jean,Dupont;785421,John,Doe
///   ```
/// - `Separators('\n', '\t')`
///   ```csv
///   id	firstname	lastname
///   865	Jean	Dupont
///   785421	John	Doe
///   ```
#[derive(Debug, Clone, Copy)]
pub struct Separators(
	/// Record separator
	///
	/// It separates the elements of a sequence.
	/// (Default value: `\n`)
	pub char,
	/// Field separator
	///
	/// It separates the fields of a structure.
	/// (Default value: `,`)
	pub char,
);
impl Default for Separators {
	#[inline(always)]
	fn default() -> Self {
		Self('\n', ',')
	}
}
impl Separators {
	/// Get the record separator
	#[inline(always)]
	pub const fn record(&self) -> char {
		self.0
	}

	/// Get the field separator
	#[inline(always)]
	pub const fn field(&self) -> char {
		self.1
	}

	fn are_unique<'s, I: IntoIterator<Item = &'s Self>>(iter: I) -> bool {
		let mut set = HashSet::new();
		for sep in iter {
			if !set.insert(sep.record()) || !set.insert(sep.field()) {
				return false;
			}
		}
		true
	}
}
impl PartialEq<Separators> for char {
	#[inline]
	fn eq(&self, sep: &Separators) -> bool {
		sep.field().eq(self) || sep.record().eq(self)
	}
}
impl<'de> Alt<&'de str, char, NomError<'de>> for Separators {
	#[inline]
	fn choice(&mut self, input: &'de str) -> IResult<&'de str, char, NomError<'de>> {
		use nom::character::complete::char;

		char(self.field())(input).or_else(|_err: NomErr<'de>| char(self.record())(input))
	}
}

macro_rules! builder_param {
	(
		$(#[$struct_attr:meta])*
		$struct:ident,
		$(
			$(#[$param_attr:meta])*
			$param:ident : $ty:ty
		),*
	) => {
		$(#[$struct_attr])*
		pub struct $struct {
			$(
				$param: $ty,
			)*
		}
		impl $struct {
			$(
				$(#[$param_attr])*
				pub fn $param<T: Into<$ty>>(mut self, $param: T) -> Self {
					self.$param = $param.into();
					self
				}
			)*
		}
	};
}
builder_param!(
	/// Builder object to create custom CSV deserializers
	///
	/// # Customising a deserializer
	/// You first need to create a new instance using [`DeserializerBuilder::default`].
	/// After that you may chain the different methods to set the deserializer's options.
	///
	/// ## Example
	/// Let us say that you want to deserialize a list of `Data` defined like:
	/// ```rust
	/// # use serde::Deserialize;
	/// #[derive(Deserialize)]
	/// #[serde(rename_all = "lowercase")]
	/// enum InnerKind {
	/// 	A,
	/// 	B,
	/// }
	/// #[derive(Deserialize)]
	/// struct Inner<'s> {
	/// 	kind: InnerKind,
	/// 	value: &'s str,
	/// }
	/// #[derive(Deserialize)]
	/// struct Data<'s> {
	/// 	id: usize,
	/// 	#[serde(borrow)]
	/// 	inner: Inner<'s>,
	/// 	check: bool,
	/// }
	/// ```
	/// And your CSV look like:
	/// ```csv
	/// 0,a:"Hello, world!",true
	///
	/// 561,b:foo bar,false
	/// ```
	/// Then you may deserialize it like this:
	/// ```rust
	/// # use serdenom_csv::de::{DeserializerBuilder, Separators};
	/// # #[derive(serde::Deserialize)] struct Data;
	/// # const INPUT: &str = "";
	/// let deserializer = DeserializerBuilder::default()
	/// 	.has_headers(false)
	/// 	.separators([Separators::default(), Separators(';', ':')]);
	///
	/// let data: Vec<Data> = deserializer.deserialize(INPUT).unwrap();
	/// ```
	///
	/// # Deserializing
	/// This builder provides two functions to finally deserialize some data:
	/// - [`deserialize`](Self::deserialize) can deserialize any `T`;
	///   but will fail should an error occur anywhre during the process.
	/// - [`deserialize_each`](Self::deserialize_each) can deserialize only record sequences;
	///   but will succeed should some errors occur while deserializing some records.
	///
	/// For more details on the two methods, please see the [module documentation](self#deserialization-methods).
	#[derive(Debug, Clone)]
	DeserializerBuilder,
	/// Define the diferent levels of nested structures in the CSV data
	///
	/// **Default value: `[Separators::default()]`**
	separators: Vec<Separators>,
	/// Character delimiting string
	///
	/// **Default value: `"`**
	string_delim: char,
	/// If the first record of the CSV input is a header row
	///
	/// Without headers, the fields in the CSV need to be declared in the same order as in your Rust struct.
	///
	/// **Default value: `true`**
	has_headers: bool
);
impl Default for DeserializerBuilder {
	#[inline]
	fn default() -> Self {
		Self {
			separators: vec![Separators::default()],
			string_delim: '"',
			has_headers: true,
		}
	}
}
impl DeserializerBuilder {
	fn extract_headers<'s>(
		&self,
		input: &'s str,
	) -> crate::Result<Option<(&'s str, Vec<&'s str>)>> {
		use nom::{
			bytes::complete::take_till1, character::complete::char, combinator::opt,
			multi::separated_list1, sequence::terminated,
		};

		if self.has_headers {
			let Separators(record_sep, field_sep) = self.separators[0];
			let (tail, row) = terminated(take_till1(|c| c == record_sep), opt(char(record_sep)))(
				input,
			)
			.map_err(|_err: NomErr<'s>| {
				crate::Error::from(DeErrorKind::SyntaxError("header row must be defined"))
			})?;
			let (_, headers) = separated_list1(char(field_sep), take_till1(|c| c == field_sep))(
				row,
			)
			.map_err(|_err: NomErr<'s>| {
				crate::Error::from(DeErrorKind::SyntaxError("header row must not be empty"))
			})?;
			Ok(Some((tail, headers)))
		} else {
			Ok(None)
		}
	}

	/// Deserialize a CSV input
	///
	/// This function returns a generic `T` such that it can deserialize anything.
	/// This means that this function will fail if an error occur anywhere during the process.
	///
	/// # Panics
	/// This function panics if the [`separators`](Self::separators) list is empty,
	/// or if they are not unique.
	///
	/// # See also
	/// - If you use the default options, you should use [`from_str`] instead.
	/// - If you want to deserialize a sequence of standard CSV records, you should use [`deserialize_each`](Self::deserialize_each) instead.
	pub fn deserialize<'de, T: Deserialize<'de>>(&self, mut input: &'de str) -> crate::Result<T> {
		assert!(!self.separators.is_empty(), "separators must not be empty");
		assert!(
			Separators::are_unique(self.separators.iter()),
			"separators must be unique"
		);
		let headers = self.extract_headers(input)?.map(|(tail, headers)| {
			input = tail;
			headers
		});
		let mut deserializer = Deserializer::new(
			input,
			headers.as_deref(),
			&self.separators,
			self.string_delim,
		);
		T::deserialize(&mut deserializer)
	}

	/// Deserialize each record of a CSV input independently
	///
	/// This function returns a `Vec<Result<T>>` such that if the deserializations of some records fail,
	/// the others will still be deserialized successfully.
	///
	/// # Panics
	/// This function panics if the [`separators`](Self::separators) list is empty,
	/// or if they are not unique.
	///
	/// # See also
	/// - If you use the default options, you should use [`from_str_each`] instead.
	/// - If you want to deserialize some non-standard CSV structure, you should use [`deserialize`](Self::deserialize) instead.
	pub fn deserialize_each<'de, T: Deserialize<'de>>(
		&self,
		mut input: &'de str,
	) -> crate::Result<Vec<crate::Result<T>>> {
		use nom::{bytes::complete::take_till, character::complete::char, multi::separated_list0};

		assert!(!self.separators.is_empty(), "separators must not be empty");
		assert!(
			Separators::are_unique(self.separators.iter()),
			"separators must be unique"
		);
		let headers = self.extract_headers(input)?.map(|(tail, headers)| {
			input = tail;
			headers
		});
		let sep = self.separators[0].record();
		let (_, records) = separated_list0::<_, _, _, NomError<'de>, _, _>(
			char(sep),
			take_till(move |c| c == sep),
		)(input.trim())
		.unwrap();
		Ok(records
			.iter()
			.filter(|record| !record.trim().is_empty())
			.map(|input| {
				let mut deserializer = Deserializer::new(
					input,
					headers.as_deref(),
					&self.separators,
					self.string_delim,
				);
				T::deserialize(&mut deserializer)
			})
			.collect())
	}
}

#[derive(Debug)]
struct Deserializer<'b, 'de> {
	input: &'de str,
	headers: Option<&'b [&'de str]>,
	separators: &'b [Separators],
	curr_separator: usize,
	curr_record: usize,
	curr_field: usize,
	string_delim: char,
}
impl<'b, 'de> Deserializer<'b, 'de> {
	#[inline(always)]
	const fn new(
		input: &'de str,
		headers: Option<&'b [&'de str]>,
		separators: &'b [Separators],
		string_delim: char,
	) -> Self {
		Self {
			input,
			headers,
			separators,
			curr_separator: 0,
			curr_record: 0,
			curr_field: 0,
			string_delim,
		}
	}

	#[inline(always)]
	fn curr_separator(&self) -> &Separators {
		&self.separators[std::cmp::min(self.curr_separator, self.separators.len() - 1)]
	}

	#[inline(always)]
	const fn is_inner(&self) -> bool {
		self.curr_separator > 0
	}

	#[inline(always)]
	const fn new_error(&self, kind: DeErrorKind) -> crate::Error {
		crate::Error::De {
			kind,
			record: Some(self.curr_record),
			field: Some(self.curr_field),
		}
	}

	fn is_prev_separator(&self, c: char) -> bool {
		for sep in self.separators[..self.curr_separator].iter().rev() {
			if c.eq(sep) {
				return true;
			}
		}
		false
	}

	#[inline]
	fn is_end_of_record(&self, c: char) -> bool {
		c == self.curr_separator().record() || self.is_prev_separator(c)
	}

	#[inline]
	fn is_end_of_field(&self, c: char) -> bool {
		c.eq(self.curr_separator()) || self.is_prev_separator(c)
	}

	#[inline]
	fn peek_end_of_seq(&self) -> bool {
		use nom::{
			character::complete::anychar,
			combinator::{eof, peek},
		};

		if eof::<_, NomError<'de>>(self.input).is_ok() {
			true
		} else if let Ok((_, c)) = peek::<_, _, NomError<'de>, _>(anychar)(self.input) {
			self.is_prev_separator(c)
		} else {
			false
		}
	}

	#[inline]
	fn peek_end_of_record(&self) -> bool {
		use nom::{character::complete::anychar, combinator::peek};

		if self.peek_end_of_seq() {
			true
		} else if let Ok((_, c)) = peek::<_, _, NomError<'de>, _>(anychar)(self.input) {
			self.is_end_of_record(c)
		} else {
			false
		}
	}

	#[inline]
	fn peek_end_of_field(&self) -> bool {
		use nom::{character::complete::anychar, combinator::peek};

		if self.peek_end_of_seq() {
			true
		} else if let Ok((_, c)) = peek::<_, _, NomError<'de>, _>(anychar)(self.input) {
			self.is_end_of_field(c)
		} else {
			false
		}
	}

	#[inline]
	fn take_end_of_record(&mut self) -> bool {
		use nom::character::complete::char;

		char::<_, NomError<'de>>(self.curr_separator().record())(self.input)
			.map(|(tail, _)| {
				self.input = tail;
			})
			.is_ok()
	}

	#[inline]
	fn take_end_of_field(&mut self) -> bool {
		use nom::character::complete::char;

		char::<_, NomError<'de>>(self.curr_separator().field())(self.input)
			.map(|(tail, _)| {
				self.input = tail;
			})
			.is_ok()
	}

	fn nom_empty_record(&mut self) -> bool {
		use nom::bytes::complete::take_till;

		let (tail, res) =
			take_till::<_, _, NomError<'de>>(|c| c == self.curr_separator().record())(self.input)
				.unwrap();
		if res.trim().is_empty() {
			self.input = tail;
			true
		} else {
			false
		}
	}

	#[inline]
	fn parse_u<N: FromStr>(&mut self) -> Result<N, NomErr<'de>>
	where
		N::Err: Debug,
	{
		use nom::character::complete::digit1;

		digit1(self.input).map(|(tail, res)| {
			self.input = tail;
			res.parse().unwrap()
		})
	}

	#[inline]
	fn parse_i<N: FromStr + Neg<Output = N>>(&mut self) -> Result<N, NomErr<'de>>
	where
		N::Err: Debug,
	{
		use nom::{
			character::complete::{char, digit1},
			combinator::opt,
			sequence::pair,
		};

		pair(opt(char('-')), digit1)(self.input).map(|(tail, (minus, res))| {
			self.input = tail;
			let res = res.parse::<N>().unwrap();
			if minus.is_some() {
				-res
			} else {
				res
			}
		})
	}
}
impl<'s, 'b, 'de> de::Deserializer<'de> for &'s mut Deserializer<'b, 'de> {
	type Error = crate::Error;

	#[inline]
	fn deserialize_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		//TODO auto type recognition
		self.deserialize_str(visitor)
	}

	#[inline]
	fn deserialize_ignored_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_str(visitor)
	}

	#[inline]
	fn deserialize_unit<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		if self.peek_end_of_field() {
			visitor.visit_unit()
		} else {
			Err(self.new_error(DeErrorKind::InvalidType("a unit type".to_string())))
		}
	}

	fn deserialize_bool<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use nom::{branch::alt, bytes::complete::tag_no_case};

		let true_str = true.to_string();
		let (tail, res) = alt((
			tag_no_case(true_str.as_str()),
			tag_no_case(false.to_string().as_str()),
		))(self.input)
		.map_err(|_err: NomErr<'de>| {
			self.new_error(DeErrorKind::InvalidType("a boolean".to_string()))
		})?;
		self.input = tail;
		visitor.visit_bool(res == true_str)
	}

	#[inline]
	fn deserialize_u8<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_u()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"an 8-bit unsigned integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_u8(val))
	}

	#[inline]
	fn deserialize_u16<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_u()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 16-bit unsigned integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_u16(val))
	}

	#[inline]
	fn deserialize_u32<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_u()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 32-bit unsigned integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_u32(val))
	}

	#[inline]
	fn deserialize_u64<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_u()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 64-bit unsigned integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_u64(val))
	}

	serde_if_integer128! {
		#[inline]
		fn deserialize_u128<V>(self, visitor: V) -> crate::Result<V::Value>
		where
			V: Visitor<'de>,
		{
			self.parse_u()
				.map_err(|_err| {
					self.new_error(DeErrorKind::InvalidType(
						"a 128-bit unsigned integer".to_string(),
					))
				})
				.and_then(|val| visitor.visit_u128(val))
		}
	}

	#[inline]
	fn deserialize_i8<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_i()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"an 8-bit signed integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_i8(val))
	}

	#[inline]
	fn deserialize_i16<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_i()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 16-bit signed integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_i16(val))
	}

	#[inline]
	fn deserialize_i32<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_i()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 32-bit signed integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_i32(val))
	}

	#[inline]
	fn deserialize_i64<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.parse_i()
			.map_err(|_err| {
				self.new_error(DeErrorKind::InvalidType(
					"a 64-bit signed integer".to_string(),
				))
			})
			.and_then(|val| visitor.visit_i64(val))
	}

	serde_if_integer128! {
		#[inline]
		fn deserialize_i128<V>(self, visitor: V) -> crate::Result<V::Value>
		where
			V: Visitor<'de>,
		{
			self.parse_i()
				.map_err(|_err| {
					self.new_error(DeErrorKind::InvalidType(
						"a 128-bit signed integer".to_string(),
					))
				})
				.and_then(|val| visitor.visit_i128(val))
		}
	}

	#[inline]
	fn deserialize_f32<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use nom::number::complete::float;

		float(self.input)
			.map(|(tail, res)| {
				self.input = tail;
				res
			})
			.map_err(|_err: NomErr<'de>| {
				self.new_error(DeErrorKind::InvalidType("a 32-bit float".to_string()))
			})
			.and_then(|val| visitor.visit_f32(val))
	}

	#[inline]
	fn deserialize_f64<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use nom::number::complete::double;

		double(self.input)
			.map(|(tail, res)| {
				self.input = tail;
				res
			})
			.map_err(|_err: NomErr<'de>| {
				self.new_error(DeErrorKind::InvalidType("a 64-bit float".to_string()))
			})
			.and_then(|val| visitor.visit_f64(val))
	}

	#[inline]
	fn deserialize_char<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use nom::character::complete::anychar;

		anychar(self.input)
			.map(|(tail, res)| {
				self.input = tail;
				res
			})
			.map_err(|_err: NomErr<'de>| {
				self.new_error(DeErrorKind::InvalidType("a character".to_string()))
			})
			.and_then(|val| visitor.visit_char(val))
	}

	fn deserialize_str<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use nom::{
			bytes::complete::{escaped, take_till},
			character::complete::{char, one_of},
			sequence::delimited,
		};

		let string_delim = self.string_delim;
		delimited(
			char(string_delim),
			escaped(
				take_till(move |c| c == string_delim),
				'\\',
				one_of(format!("{}n\\", string_delim).as_str()),
			),
			char(string_delim),
		)(self.input)
		.map(|(tail, res)| {
			self.input = tail;
			res
		})
		.or_else(|_err: NomErr<'de>| {
			let (tail, res) =
				take_till::<_, _, NomError<'de>>(|c: char| self.is_end_of_field(c))(self.input)
					.unwrap();
			self.input = tail;
			Ok(res)
		})
		.and_then(|val| visitor.visit_borrowed_str(val))
	}

	#[inline]
	fn deserialize_string<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_str(visitor)
	}

	#[inline]
	fn deserialize_identifier<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_str(visitor)
	}

	#[inline]
	fn deserialize_option<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		if self.peek_end_of_field() {
			visitor.visit_none()
		} else {
			visitor.visit_some(self)
		}
	}

	#[inline]
	fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_unit(visitor)
	}

	#[inline]
	fn deserialize_newtype_struct<V>(
		self,
		_name: &'static str,
		visitor: V,
	) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_newtype_struct(self)
	}

	#[inline]
	fn deserialize_map<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_map(StructAccess::from(self))
	}

	fn deserialize_struct<V>(
		self,
		_name: &'static str,
		fields: &'static [&'static str],
		visitor: V,
	) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		if self.is_inner() || self.headers.is_none() {
			let mut fields = Some(fields);
			macro_rules! swap_headers {
				() => {
					std::mem::swap(&mut self.headers, &mut fields);
				};
			}
			swap_headers!();
			let res = self.deserialize_map(visitor);
			swap_headers!();
			res
		} else {
			self.deserialize_map(visitor)
		}
	}

	#[inline]
	fn deserialize_enum<V>(
		self,
		_name: &'static str,
		_variants: &'static [&'static str],
		visitor: V,
	) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_enum(EnumAccess::from(self))
	}

	#[inline]
	fn deserialize_seq<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_seq(StructAccess::from(self))
	}

	#[inline]
	fn deserialize_tuple<V>(self, len: usize, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_seq(TupleAccess::new(self, len))
	}

	#[inline]
	fn deserialize_tuple_struct<V>(
		self,
		_name: &'static str,
		len: usize,
		visitor: V,
	) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_tuple(len, visitor)
	}

	#[inline]
	fn deserialize_bytes<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_seq(visitor)
	}

	#[inline]
	fn deserialize_byte_buf<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		self.deserialize_bytes(visitor)
	}
}

#[derive(Debug)]
struct HeaderDeserializer<'b, H> {
	headers: &'b [H],
	index: usize,
}
impl<'s, 'b, 'de> de::Deserializer<'de> for &'s mut HeaderDeserializer<'b, &'b str> {
	type Error = crate::Error;

	#[inline]
	fn deserialize_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_str(self.headers[self.index])
	}

	forward_to_deserialize_any! {
		ignored_any unit
		bool u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 char str string identifier
		option unit_struct newtype_struct map struct enum seq tuple tuple_struct bytes byte_buf
	}
}
impl<'s, 'b, 'de> de::Deserializer<'de> for &'s mut HeaderDeserializer<'b, usize> {
	type Error = crate::Error;

	#[cfg(target_pointer_width = "16")]
	#[inline]
	fn deserialize_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_u16(self.headers[self.index] as u16)
	}

	#[cfg(target_pointer_width = "32")]
	#[inline]
	fn deserialize_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_u32(self.headers[self.index] as u32)
	}

	#[cfg(target_pointer_width = "64")]
	#[inline]
	fn deserialize_any<V>(self, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		visitor.visit_u64(self.headers[self.index] as u64)
	}

	forward_to_deserialize_any! {
		ignored_any unit
		bool u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 char str string identifier
		option unit_struct newtype_struct map struct enum seq tuple tuple_struct bytes byte_buf
	}
}

macro_rules! next_key {
	($access:expr, $len:expr) => {
		let access: &mut StructAccess = $access;
		let len: usize = $len;
		macro_rules! return_structure_error {
			() => {
				return Err(access.deserializer.new_error(DeErrorKind::StructureError(
					access.deserializer.curr_field,
					Some(len),
				)));
			};
		}
		if access.deserializer.peek_end_of_record() {
			if access.counter == len {
				return Ok(None);
			} else {
				return_structure_error!();
			}
		}
		if access.counter > len {
			return_structure_error!();
		}
	};
}
macro_rules! take_end_of_field {
	($deserializer:expr) => {
		let deserializer: &mut $crate::de::Deserializer = $deserializer;
		if !deserializer.take_end_of_field() {
			return Err(deserializer.new_error(DeErrorKind::SyntaxError("field separator")));
		}
	};
}

#[derive(Debug)]
struct StructAccess<'r, 'b, 'de> {
	deserializer: &'r mut Deserializer<'b, 'de>,
	counter: usize,
}
impl<'r, 'b, 'de> From<&'r mut Deserializer<'b, 'de>> for StructAccess<'r, 'b, 'de> {
	#[inline(always)]
	fn from(deserializer: &'r mut Deserializer<'b, 'de>) -> Self {
		Self {
			deserializer,
			counter: 0,
		}
	}
}
impl<'r, 'b, 'de> SeqAccess<'de> for StructAccess<'r, 'b, 'de> {
	type Error = crate::Error;

	fn next_element_seed<S>(&mut self, seed: S) -> crate::Result<Option<S::Value>>
	where
		S: DeserializeSeed<'de>,
	{
		let mut empty_record = true;
		let mut empty_counter = 0usize;
		while empty_record {
			if self.deserializer.peek_end_of_seq() {
				return Ok(None);
			}
			if (self.counter > 0 || empty_counter > 0) && !self.deserializer.take_end_of_record() {
				return Err(self
					.deserializer
					.new_error(DeErrorKind::SyntaxError("record separator")));
			}
			empty_record = self.deserializer.nom_empty_record();
			empty_counter += 1;
		}
		self.counter += 1;
		if self.deserializer.curr_separator == 0 {
			self.deserializer.curr_field = 0;
			self.deserializer.curr_record += 1;
		}
		seed.deserialize(&mut *self.deserializer).map(Some)
	}
}
impl<'r, 'b, 'de> MapAccess<'de> for StructAccess<'r, 'b, 'de> {
	type Error = crate::Error;

	fn next_key_seed<S>(&mut self, seed: S) -> crate::Result<Option<S::Value>>
	where
		S: DeserializeSeed<'de>,
	{
		let len = self.deserializer.headers.unwrap().len();
		next_key!(self, len);
		self.deserializer
			.headers
			.ok_or_else(|| self.deserializer.new_error(DeErrorKind::NeedHeaders))
			.and_then(|headers| {
				let mut header_deserializer = HeaderDeserializer {
					headers,
					index: self.counter,
				};
				seed.deserialize(&mut header_deserializer).map(Some)
			})
	}

	fn next_value_seed<S>(&mut self, seed: S) -> crate::Result<S::Value>
	where
		S: DeserializeSeed<'de>,
	{
		if self.counter > 0 {
			take_end_of_field!(self.deserializer);
		}
		self.counter += 1;
		if !self.deserializer.is_inner() {
			self.deserializer.curr_field += 1;
		}
		self.deserializer.curr_separator += 1;
		let res = seed.deserialize(&mut *self.deserializer);
		self.deserializer.curr_separator -= 1;
		res
	}

	#[inline]
	fn size_hint(&self) -> Option<usize> {
		Some(self.deserializer.headers?.len() - self.counter)
	}
}

#[derive(Debug)]
struct TupleAccess<'r, 'b, 'de> {
	access: StructAccess<'r, 'b, 'de>,
	len: usize,
}
impl<'r, 'b, 'de> TupleAccess<'r, 'b, 'de> {
	#[inline]
	fn new(deserializer: &'r mut Deserializer<'b, 'de>, len: usize) -> Self {
		Self {
			access: StructAccess::from(deserializer),
			len,
		}
	}
}
impl<'r, 'b, 'de> SeqAccess<'de> for TupleAccess<'r, 'b, 'de> {
	type Error = crate::Error;

	fn next_element_seed<S>(&mut self, seed: S) -> crate::Result<Option<S::Value>>
	where
		S: DeserializeSeed<'de>,
	{
		next_key!(&mut self.access, self.len);
		self.access.next_value_seed(seed).map(Some)
	}

	#[inline]
	fn size_hint(&self) -> Option<usize> {
		Some(self.len - self.access.counter)
	}
}

#[derive(Debug)]
struct EnumAccess<'r, 'b, 'de> {
	deserializer: &'r mut Deserializer<'b, 'de>,
}
impl<'r, 'b, 'de> From<&'r mut Deserializer<'b, 'de>> for EnumAccess<'r, 'b, 'de> {
	#[inline(always)]
	fn from(deserializer: &'r mut Deserializer<'b, 'de>) -> Self {
		Self { deserializer }
	}
}
impl<'r, 'b, 'de> de::EnumAccess<'de> for EnumAccess<'r, 'b, 'de> {
	type Error = crate::Error;
	type Variant = Self;

	#[inline]
	fn variant_seed<S>(self, seed: S) -> crate::Result<(S::Value, Self::Variant)>
	where
		S: DeserializeSeed<'de>,
	{
		Ok((seed.deserialize(&mut *self.deserializer)?, self))
	}
}
impl<'r, 'b, 'de> VariantAccess<'de> for EnumAccess<'r, 'b, 'de> {
	type Error = crate::Error;

	#[inline(always)]
	fn unit_variant(self) -> crate::Result<()> {
		Ok(())
	}

	fn newtype_variant_seed<S>(self, seed: S) -> crate::Result<S::Value>
	where
		S: DeserializeSeed<'de>,
	{
		take_end_of_field!(self.deserializer);
		seed.deserialize(self.deserializer)
	}

	fn tuple_variant<V>(self, len: usize, visitor: V) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use serde::de::Deserializer;

		take_end_of_field!(self.deserializer);
		self.deserializer.deserialize_tuple(len, visitor)
	}

	fn struct_variant<V>(
		self,
		fields: &'static [&'static str],
		visitor: V,
	) -> crate::Result<V::Value>
	where
		V: Visitor<'de>,
	{
		use serde::de::Deserializer;

		take_end_of_field!(self.deserializer);
		self.deserializer
			.deserialize_struct(Default::default(), fields, visitor)
	}
}
