# `serdenom_csv`
> [Serde](https://serde.rs/) library for CSV (with nested structures) parsing with [Nom](https://docs.rs/nom)

I created this library because I needed a CSV parser which can handle nested structures in one of my projects.
So I just took the opportunity to program a whole Serde library with the help of Nom to parse CSV.

## Deserializer
- [**Supports all types**](de#supported-types) (including structs, enums and sequence-like types)
- [Supports borrowed strings](de#borrowed-types)
- Customizable deserializer using [`DeserializerBuilder`](de::DeserializerBuilder)

### Example
This CSV with a list of phone numbers:
```csv
id,firstname,lastname,phones,ssn
8452,John,Doe,0123456789;0987654321,
95674,Jean,Dupont,,5214511445215
```
can be deserialized like this:
```rust
use serde::Deserialize;
use serdenom_csv::de::{DeserializerBuilder, Separators};

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct Person<'s> {
    id: usize,
    firstname: &'s str,
    lastname: &'s str,
    phones: Vec<&'s str>,
    ssn: Option<u64>,
}

const INPUT: &str = r#"id,firstname,lastname,phones,ssn
8452,John,Doe,0123456789;0987654321,
95674,Jean,Dupont,,5214511445215"#;

let deserializer = DeserializerBuilder::default()
    .separators([Separators::default(), Separators(';', ':')]);
let data: Vec<Person> = deserializer.deserialize(INPUT).unwrap();

assert_eq!(data[0], Person {
    id: 8452,
    firstname: "John",
    lastname: "Doe",
    phones: vec!["0123456789", "0987654321"],
    ssn: None,
});
assert_eq!(data[1], Person {
    id: 95674,
    firstname: "Jean",
    lastname: "Dupont",
    phones: vec![],
    ssn: Some(5214511445215),
});
```

Please read the [`de` module documentation](de) for more details.

## Serializer
`serdenom_csv` does not provide a serializer yet.
