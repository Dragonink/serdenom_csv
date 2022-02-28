#![allow(clippy::unit_cmp, clippy::bool_assert_comparison)]
use serde::Deserialize;
use serdenom_csv::{de::*, from_str, from_str_each};

#[test]
fn primitives() {
	#[derive(Debug, PartialEq, Deserialize)]
	struct S<'s> {
		unit: (),
		boolean: bool,
		integer: i64,
		float: f32,
		string: &'s str,
		#[serde(default)]
		default_boolean: bool,
		option_string: Option<&'s str>,
	}
	const DATASET: &str = include_str!("primitives.csv");

	let data: Vec<S> = from_str(DATASET).unwrap();
	assert_eq!(data.len(), 2);
	let other_data: Vec<S> = from_str_each(DATASET)
		.unwrap()
		.into_iter()
		.map(|res| res.unwrap())
		.collect();
	assert_eq!(data, other_data);

	assert_eq!(
		data[0],
		S {
			unit: (),
			boolean: true,
			integer: 71,
			float: 23.9774,
			string: "nicole exactly trainers weather minimum",
			default_boolean: Default::default(),
			option_string: Some("foo bar baz"),
		}
	);
	assert_eq!(
		data[1],
		S {
			unit: (),
			boolean: false,
			integer: -90,
			float: 28.3797,
			string: "Hello, world!",
			default_boolean: Default::default(),
			option_string: None,
		}
	);
}

#[test]
fn inner_struct() {
	#[derive(Debug, PartialEq, Eq, Deserialize)]
	struct Si {
		c: char,
		num: usize,
	}
	#[derive(Debug, PartialEq, Eq, Deserialize)]
	struct S {
		id: usize,
		list: Vec<Si>,
	}
	const DATASET: &str = include_str!("inner_struct.csv");

	let deserializer =
		DeserializerBuilder::default().separators([Separators::default(), Separators(';', ':')]);

	let data: Vec<S> = deserializer.deserialize(DATASET).unwrap();
	assert_eq!(data.len(), 3);
	let other_data: Vec<S> = deserializer
		.deserialize_each(DATASET)
		.unwrap()
		.into_iter()
		.map(|res| res.unwrap())
		.collect();
	assert_eq!(data, other_data);

	assert_eq!(
		data[0],
		S {
			id: 0,
			list: vec![Si { c: 'a', num: 42 }, Si { c: 'b', num: 69 }],
		}
	);
	assert_eq!(
		data[1],
		S {
			id: 1,
			list: vec![],
		}
	);
	assert_eq!(
		data[2],
		S {
			id: 2,
			list: vec![Si { c: 'c', num: 85412 }],
		}
	);
}

#[test]
fn tuple() {
	#[derive(Debug, PartialEq, Eq, Deserialize)]
	struct T<'s>(usize, &'s str, Option<bool>);
	const DATASET: &str = include_str!("tuple.csv");

	let deserializer = DeserializerBuilder::default().has_headers(false);

	let data: Vec<T> = deserializer.deserialize(DATASET).unwrap();
	assert_eq!(data.len(), 3);
	let other_data: Vec<T> = deserializer
		.deserialize_each(DATASET)
		.unwrap()
		.into_iter()
		.map(|res| res.unwrap())
		.collect();
	assert_eq!(data, other_data);

	assert_eq!(data[0], T(0, "Hello, world!", Some(true)));
	assert_eq!(data[1], T(42, "foo bar", None));
	assert_eq!(data[2], T(69, "yeah.", Some(false)));
}

#[test]
fn r#enum() {
	#[derive(Debug, PartialEq, Eq, Deserialize)]
	#[serde(rename_all = "lowercase")]
	enum E<'s> {
		Unit,
		NewType(usize),
		Tuple(bool, &'s str),
		Struct { num: usize, text: &'s str },
	}
	#[derive(Debug, PartialEq, Eq, Deserialize)]
	struct S<'s> {
		id: usize,
		#[serde(borrow)]
		data: E<'s>,
	}
	const DATASET: &str = include_str!("enum.csv");

	let deserializer = DeserializerBuilder::default()
		.has_headers(false)
		.separators([Separators::default(), Separators(';', ':')]);

	let data: Vec<S> = deserializer.deserialize(DATASET).unwrap();
	assert_eq!(data.len(), 4);
	let other_data: Vec<S> = deserializer
		.deserialize_each(DATASET)
		.unwrap()
		.into_iter()
		.map(|res| res.unwrap())
		.collect();
	assert_eq!(data, other_data);

	assert_eq!(
		data[0],
		S {
			id: 0,
			data: E::Unit,
		}
	);
	assert_eq!(
		data[1],
		S {
			id: 1,
			data: E::NewType(42),
		}
	);
	assert_eq!(
		data[2],
		S {
			id: 2,
			data: E::Tuple(true, "Hello, world!"),
		}
	);
	assert_eq!(
		data[3],
		S {
			id: 4,
			data: E::Struct {
				num: 69,
				text: "foo bar",
			},
		}
	);
}
