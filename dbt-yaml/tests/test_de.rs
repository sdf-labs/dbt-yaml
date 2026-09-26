#![allow(
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::derive_partial_eq_without_eq,
    clippy::similar_names,
    clippy::uninlined_format_args
)]

use dbt_yaml::{Deserializer, Number, Value};
use indoc::indoc;
use serde_derive::Deserialize;
use std::collections::BTreeMap;
use std::fmt::Debug;

fn test_de<T>(yaml: &str, expected: &T)
where
    T: serde::de::DeserializeOwned + PartialEq + Debug,
{
    let deserialized: T = dbt_yaml::from_str(yaml).unwrap();
    assert_eq!(*expected, deserialized);

    let value: Value = dbt_yaml::from_str(yaml).unwrap();
    let deserialized = T::deserialize(&value).unwrap();
    assert_eq!(*expected, deserialized);

    let deserialized: T = dbt_yaml::from_value(value).unwrap();
    assert_eq!(*expected, deserialized);

    dbt_yaml::from_str::<serde::de::IgnoredAny>(yaml).unwrap();

    let mut deserializer = Deserializer::from_str(yaml);
    let document = deserializer.next().unwrap();
    let deserialized = T::deserialize(document).unwrap();
    assert_eq!(*expected, deserialized);
    assert!(deserializer.next().is_none());
}

fn test_de_no_value<'de, T>(yaml: &'de str, expected: &T)
where
    T: serde::de::Deserialize<'de> + PartialEq + Debug,
{
    let deserialized: T = dbt_yaml::from_str(yaml).unwrap();
    assert_eq!(*expected, deserialized);

    dbt_yaml::from_str::<dbt_yaml::Value>(yaml).unwrap();
    dbt_yaml::from_str::<serde::de::IgnoredAny>(yaml).unwrap();
}

fn test_de_seed<'de, T, S>(yaml: &'de str, seed: S, expected: &T)
where
    T: PartialEq + Debug,
    S: serde::de::DeserializeSeed<'de, Value = T>,
{
    let deserialized: T = seed.deserialize(Deserializer::from_str(yaml)).unwrap();
    assert_eq!(*expected, deserialized);

    dbt_yaml::from_str::<dbt_yaml::Value>(yaml).unwrap();
    dbt_yaml::from_str::<serde::de::IgnoredAny>(yaml).unwrap();
}

#[test]
fn test_borrowed() {
    let yaml = indoc! {"
        - plain nonàscii
        - 'single quoted'
        - \"double quoted\"
    "};
    let expected = vec!["plain nonàscii", "single quoted", "double quoted"];
    test_de_no_value(yaml, &expected);
}

#[test]
fn test_alias() {
    let yaml = indoc! {"
        first:
          &alias
          1
        second:
          *alias
        third: 3
    "};
    let mut expected = BTreeMap::new();
    expected.insert("first".to_owned(), 1);
    expected.insert("second".to_owned(), 1);
    expected.insert("third".to_owned(), 3);
    test_de(yaml, &expected);
}

#[cfg(feature = "yaml_11")]
#[test]
fn test_number_underscores() {
    let yaml = indoc! {"
        - 1_000
        - \"1_000\"
        - 0xa_beef
        - 0b1_0000
    "};
    let expected = vec![
        Value::number(Number::from(1000)),
        Value::string("1_000".to_owned()),
        Value::number(Number::from(0xABEEF)),
        Value::number(Number::from(0b10000)),
    ];
    test_de(yaml, &expected);
}

#[test]
fn test_option() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Data {
        a: Option<f64>,
        b: Option<String>,
        c: Option<bool>,
    }
    let yaml = indoc! {"
        b:
        c: true
    "};
    let expected = Data {
        a: None,
        b: None,
        c: Some(true),
    };
    test_de(yaml, &expected);
}

#[test]
fn test_option_alias() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Data {
        a: Option<f64>,
        b: Option<String>,
        c: Option<bool>,
        d: Option<f64>,
        e: Option<String>,
        f: Option<bool>,
    }
    let yaml = indoc! {"
        none_f:
          &none_f
          ~
        none_s:
          &none_s
          ~
        none_b:
          &none_b
          ~

        some_f:
          &some_f
          1.0
        some_s:
          &some_s
          x
        some_b:
          &some_b
          true

        a: *none_f
        b: *none_s
        c: *none_b
        d: *some_f
        e: *some_s
        f: *some_b
    "};
    let expected = Data {
        a: None,
        b: None,
        c: None,
        d: Some(1.0),
        e: Some("x".to_owned()),
        f: Some(true),
    };
    test_de(yaml, &expected);
}

#[test]
fn test_enum_alias() {
    #[derive(Deserialize, PartialEq, Debug)]
    enum E {
        A,
        B(u8, u8),
    }
    #[derive(Deserialize, PartialEq, Debug)]
    struct Data {
        a: E,
        b: E,
    }
    let yaml = indoc! {"
        aref:
          &aref
          A
        bref:
          &bref
          !B
            - 1
            - 2

        a: *aref
        b: *bref
    "};
    let expected = Data {
        a: E::A,
        b: E::B(1, 2),
    };
    test_de(yaml, &expected);
}

#[test]
fn test_enum_representations() {
    #[derive(Deserialize, PartialEq, Debug)]
    enum Enum {
        Unit,
        Tuple(i32, i32),
        Struct { x: i32, y: i32 },
        String(String),
        Number(f64),
    }

    let yaml = indoc! {"
        - Unit
        - 'Unit'
        - !Unit
        - !Unit ~
        - !Unit null
        - !Tuple [0, 0]
        - !Tuple
          - 0
          - 0
        - !Struct {x: 0, y: 0}
        - !Struct
          x: 0
          y: 0
        - !String '...'
        - !String ...
        - !Number 0
    "};

    let expected = vec![
        Enum::Unit,
        Enum::Unit,
        Enum::Unit,
        Enum::Unit,
        Enum::Unit,
        Enum::Tuple(0, 0),
        Enum::Tuple(0, 0),
        Enum::Struct { x: 0, y: 0 },
        Enum::Struct { x: 0, y: 0 },
        Enum::String("...".to_owned()),
        Enum::String("...".to_owned()),
        Enum::Number(0.0),
    ];

    test_de(yaml, &expected);

    let yaml = indoc! {"
        - !String
    "};
    let expected = vec![Enum::String(String::new())];
    test_de_no_value(yaml, &expected);
}

#[test]
fn test_number_as_string() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Num {
        value: String,
    }
    let yaml = indoc! {"
        # Cannot be represented as u128
        value: 340282366920938463463374607431768211457
    "};
    let expected = Num {
        value: "340282366920938463463374607431768211457".to_owned(),
    };
    test_de_no_value(yaml, &expected);
}

#[test]
fn test_empty_string() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Struct {
        empty: String,
        tilde: String,
    }
    let yaml = indoc! {"
        empty:
        tilde: ~
    "};
    let expected = Struct {
        empty: String::new(),
        tilde: "~".to_owned(),
    };
    test_de_no_value(yaml, &expected);
}

#[test]
fn test_i128_big() {
    let expected: i128 = i64::MIN as i128 - 1;
    let yaml = indoc! {"
        -9223372036854775809
    "};
    assert_eq!(expected, dbt_yaml::from_str::<i128>(yaml).unwrap());

    let octal = indoc! {"
        -0o1000000000000000000001
    "};
    assert_eq!(expected, dbt_yaml::from_str::<i128>(octal).unwrap());
}

#[test]
fn test_u128_big() {
    let expected: u128 = u64::MAX as u128 + 1;
    let yaml = indoc! {"
        18446744073709551616
    "};
    assert_eq!(expected, dbt_yaml::from_str::<u128>(yaml).unwrap());

    let octal = indoc! {"
        0o2000000000000000000000
    "};
    assert_eq!(expected, dbt_yaml::from_str::<u128>(octal).unwrap());
}

#[test]
fn test_number_alias_as_string() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Num {
        version: String,
        value: String,
    }
    let yaml = indoc! {"
        version: &a 1.10
        value: *a
    "};
    let expected = Num {
        version: "1.10".to_owned(),
        value: "1.10".to_owned(),
    };
    test_de_no_value(yaml, &expected);
}

#[test]
fn test_de_mapping() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        pub substructure: dbt_yaml::Mapping,
    }
    let yaml = indoc! {"
        substructure:
          a: 'foo'
          b: 'bar'
    "};

    let mut expected = Data {
        substructure: dbt_yaml::Mapping::new(),
    };
    expected.substructure.insert(
        dbt_yaml::Value::string("a".to_owned()),
        dbt_yaml::Value::string("foo".to_owned()),
    );
    expected.substructure.insert(
        dbt_yaml::Value::string("b".to_owned()),
        dbt_yaml::Value::string("bar".to_owned()),
    );

    test_de(yaml, &expected);
}

#[test]
fn test_byte_order_mark() {
    let yaml = "\u{feff}- 0\n";
    let expected = vec![0];
    test_de(yaml, &expected);
}

#[test]
fn test_bomb() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        expected: String,
    }

    // This would deserialize an astronomical number of elements if we were
    // vulnerable.
    let yaml = indoc! {"
        a: &a ~
        b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
        c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
        d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
        e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]
        f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]
        g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]
        h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]
        i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]
        j: &j [*i,*i,*i,*i,*i,*i,*i,*i,*i]
        k: &k [*j,*j,*j,*j,*j,*j,*j,*j,*j]
        l: &l [*k,*k,*k,*k,*k,*k,*k,*k,*k]
        m: &m [*l,*l,*l,*l,*l,*l,*l,*l,*l]
        n: &n [*m,*m,*m,*m,*m,*m,*m,*m,*m]
        o: &o [*n,*n,*n,*n,*n,*n,*n,*n,*n]
        p: &p [*o,*o,*o,*o,*o,*o,*o,*o,*o]
        q: &q [*p,*p,*p,*p,*p,*p,*p,*p,*p]
        r: &r [*q,*q,*q,*q,*q,*q,*q,*q,*q]
        s: &s [*r,*r,*r,*r,*r,*r,*r,*r,*r]
        t: &t [*s,*s,*s,*s,*s,*s,*s,*s,*s]
        u: &u [*t,*t,*t,*t,*t,*t,*t,*t,*t]
        v: &v [*u,*u,*u,*u,*u,*u,*u,*u,*u]
        w: &w [*v,*v,*v,*v,*v,*v,*v,*v,*v]
        x: &x [*w,*w,*w,*w,*w,*w,*w,*w,*w]
        y: &y [*x,*x,*x,*x,*x,*x,*x,*x,*x]
        z: &z [*y,*y,*y,*y,*y,*y,*y,*y,*y]
        expected: string
    "};

    let expected = Data {
        expected: "string".to_owned(),
    };

    assert_eq!(expected, dbt_yaml::from_str::<Data>(yaml).unwrap());
}

#[test]
fn test_numbers() {
    let cases = [
        ("0xF0", "240"),
        ("+0xF0", "240"),
        ("-0xF0", "-240"),
        ("0o70", "56"),
        ("+0o70", "56"),
        ("-0o70", "-56"),
        ("0b10", "2"),
        ("+0b10", "2"),
        ("-0b10", "-2"),
        ("127", "127"),
        ("+127", "127"),
        ("-127", "-127"),
        (".inf", ".inf"),
        (".Inf", ".inf"),
        (".INF", ".inf"),
        ("-.inf", "-.inf"),
        ("-.Inf", "-.inf"),
        ("-.INF", "-.inf"),
        (".nan", ".nan"),
        (".NaN", ".nan"),
        (".NAN", ".nan"),
        ("0.1", "0.1"),
    ];
    for &(yaml, expected) in &cases {
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        match value {
            Value::Number(number, ..) => assert_eq!(number.to_string(), expected),
            _ => panic!("expected number. input={:?}, result={:?}", yaml, value),
        }
    }

    // NOT numbers.
    #[allow(unused_mut)]
    let mut cases = vec![
        "0127", "+0127", "-0127", "++.inf", "+-.inf", "++1", "+-1", "-+1", "--1", "0x+1", "0x-1",
        "-0x+1", "-0x-1", "++0x1", "+-0x1", "-+0x1", "--0x1",
    ];
    #[cfg(not(feature = "yaml_11"))]
    {
        cases.extend(["1_1", "1_1.0"]);
    }

    for yaml in &cases {
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        match value {
            Value::String(string, ..) => assert_eq!(string, *yaml),
            _ => panic!("expected string. input={:?}, result={:?}", yaml, value),
        }
    }
}

#[cfg(feature = "yaml_11")]
#[test]
fn test_yaml11_numbers() {
    let cases = [
        ("0x1_0000", "65536"),
        ("0o1_0000", "4096"),
        ("0b1_0000", "16"),
        ("1__00_0", "1000"),
        ("1_000.0", "1000.0"),
        // Note: scientific notation is apparently not supported by the Python
        // yaml parser: ("1_000e_-3", "1.0"),
        ("-.1_000", "-0.1"),
    ];
    for &(yaml, expected) in &cases {
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        match value {
            Value::Number(number, ..) => assert_eq!(number.to_string(), expected),
            _ => panic!("expected number. input={:?}, result={:?}", yaml, value),
        }
    }

    // NOT numbers.
    let cases = ["_1", "+_1", "-_1", "_0x1", "0_x1", "-_3", "._inf"];
    for yaml in &cases {
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        match value {
            Value::String(string, ..) => assert_eq!(string, *yaml),
            _ => panic!("expected string. input={:?}, result={:?}", yaml, value),
        }
    }
}

#[test]
fn test_nan() {
    // There is no negative NaN in YAML.
    assert!(dbt_yaml::from_str::<f32>(".nan")
        .unwrap()
        .is_sign_positive());
    assert!(dbt_yaml::from_str::<f64>(".nan")
        .unwrap()
        .is_sign_positive());
}

#[test]
fn test_stateful() {
    struct Seed(i64);

    impl<'de> serde::de::DeserializeSeed<'de> for Seed {
        type Value = i64;
        fn deserialize<D>(self, deserializer: D) -> Result<i64, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            struct Visitor(i64);
            impl serde::de::Visitor<'_> for Visitor {
                type Value = i64;

                fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(formatter, "an integer")
                }

                fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<i64, E> {
                    Ok(v * self.0)
                }

                fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<i64, E> {
                    Ok(v as i64 * self.0)
                }
            }

            deserializer.deserialize_any(Visitor(self.0))
        }
    }

    let cases = [("3", 5, 15), ("6", 7, 42), ("-5", 9, -45)];
    for &(yaml, seed, expected) in &cases {
        test_de_seed(yaml, Seed(seed), &expected);
    }
}

#[test]
fn test_ignore_tag() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        struc: Struc,
        tuple: Tuple,
        newtype: Newtype,
        map: BTreeMap<char, usize>,
        vec: Vec<usize>,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Struc {
        x: usize,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Tuple(usize, usize);

    #[derive(Deserialize, Debug, PartialEq)]
    struct Newtype(usize);

    let yaml = indoc! {"
        struc: !wat
          x: 0
        tuple: !wat
          - 0
          - 0
        newtype: !wat 0
        map: !wat
          x: 0
        vec: !wat
          - 0
    "};

    let expected = Data {
        struc: Struc { x: 0 },
        tuple: Tuple(0, 0),
        newtype: Newtype(0),
        map: {
            let mut map = BTreeMap::new();
            map.insert('x', 0);
            map
        },
        vec: vec![0],
    };

    test_de(yaml, &expected);
}

#[test]
fn test_no_required_fields() {
    #[derive(Deserialize, PartialEq, Debug)]
    pub struct NoRequiredFields {
        optional: Option<usize>,
    }

    for document in ["", "# comment\n"] {
        let expected = NoRequiredFields { optional: None };
        let deserialized: NoRequiredFields = dbt_yaml::from_str(document).unwrap();
        assert_eq!(expected, deserialized);

        let expected = Vec::<String>::new();
        let deserialized: Vec<String> = dbt_yaml::from_str(document).unwrap();
        assert_eq!(expected, deserialized);

        let expected = BTreeMap::new();
        let deserialized: BTreeMap<char, usize> = dbt_yaml::from_str(document).unwrap();
        assert_eq!(expected, deserialized);

        let expected = None;
        let deserialized: Option<String> = dbt_yaml::from_str(document).unwrap();
        assert_eq!(expected, deserialized);

        let expected = Value::null();
        let deserialized: Value = dbt_yaml::from_str(document).unwrap();
        assert_eq!(expected, deserialized);
    }
}

#[test]
fn test_empty_scalar() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Struct<T> {
        thing: T,
    }

    let yaml = "thing:\n";
    let expected = Struct {
        thing: dbt_yaml::Sequence::new(),
    };
    test_de(yaml, &expected);

    let expected = Struct {
        thing: dbt_yaml::Mapping::new(),
    };
    test_de(yaml, &expected);
}

#[test]
fn test_python_safe_dump() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Frob {
        foo: u32,
    }

    // This matches output produced by PyYAML's `yaml.safe_dump` when using the
    // default_style parameter.
    //
    //    >>> import yaml
    //    >>> d = {"foo": 7200}
    //    >>> print(yaml.safe_dump(d, default_style="|"))
    //    "foo": !!int |-
    //      7200
    //
    let yaml = indoc! {r#"
        "foo": !!int |-
            7200
    "#};

    let expected = Frob { foo: 7200 };
    test_de(yaml, &expected);
}

#[test]
fn test_tag_resolution() {
    // With the `yaml_11` feature the YAML 1.1 words resolve as booleans,
    // matching PyYAML's implicit resolver; without it they stay strings per the
    // YAML 1.2 core schema.
    #[cfg(feature = "yaml_11")]
    fn yaml_11_bool_12_str(_s: &str, boolean: bool) -> Value {
        Value::bool(boolean)
    }

    #[cfg(not(feature = "yaml_11"))]
    fn yaml_11_bool_12_str(s: &str, _boolean: bool) -> Value {
        Value::string(s.to_owned())
    }

    // https://yaml.org/spec/1.2.2/#1032-tag-resolution
    let yaml = indoc! {"
        - null
        - Null
        - NULL
        - ~
        -
        - true
        - True
        - TRUE
        - false
        - False
        - FALSE
        - y
        - Y
        - yes
        - Yes
        - YES
        - n
        - N
        - no
        - No
        - NO
        - on
        - On
        - ON
        - off
        - Off
        - OFF
    "};

    let expected = vec![
        Value::null(),
        Value::null(),
        Value::null(),
        Value::null(),
        Value::null(),
        Value::bool(true),
        Value::bool(true),
        Value::bool(true),
        Value::bool(false),
        Value::bool(false),
        Value::bool(false),
        Value::string("y".to_owned()),
        Value::string("Y".to_owned()),
        yaml_11_bool_12_str("yes", true),
        yaml_11_bool_12_str("Yes", true),
        yaml_11_bool_12_str("YES", true),
        Value::string("n".to_owned()),
        Value::string("N".to_owned()),
        yaml_11_bool_12_str("no", false),
        yaml_11_bool_12_str("No", false),
        yaml_11_bool_12_str("NO", false),
        yaml_11_bool_12_str("on", true),
        yaml_11_bool_12_str("On", true),
        yaml_11_bool_12_str("ON", true),
        yaml_11_bool_12_str("off", false),
        yaml_11_bool_12_str("Off", false),
        yaml_11_bool_12_str("OFF", false),
    ];

    test_de(yaml, &expected);
}

#[cfg(feature = "yaml_11")]
#[test]
fn test_yaml11_booleans() {
    // Matches PyYAML's implicit resolver: yes/no/on/off in three casings are
    // resolved to booleans; the YAML 1.1 spec's single-letter y/n are kept as
    // strings, as are mixed casings like YeS.
    let yaml = indoc! {"
        - yes
        - No
        - ON
        - off
        - y
        - n
        - YeS
    "};
    let expected = vec![
        Value::bool(true),
        Value::bool(false),
        Value::bool(true),
        Value::bool(false),
        Value::string("y".to_owned()),
        Value::string("n".to_owned()),
        Value::string("YeS".to_owned()),
    ];
    test_de(yaml, &expected);

    // An explicit !!bool tag accepts any casing, as in PyYAML's
    // construct_yaml_bool...
    let yaml = indoc! {"
        - !!bool YeS
        - !!bool oFF
    "};
    let expected = vec![Value::bool(true), Value::bool(false)];
    test_de(yaml, &expected);

    // ...but y/n are not booleans even with the tag.
    assert!(dbt_yaml::from_str::<Value>("!!bool y").is_err());

    // Bool-typed fields resolve the YAML 1.1 words, untagged and tagged.
    #[derive(Deserialize, PartialEq, Debug)]
    struct Data {
        untagged: bool,
        tagged: bool,
    }
    let yaml = indoc! {"
        untagged: on
        tagged: !!bool YeS
    "};
    let expected = Data {
        untagged: true,
        tagged: true,
    };
    test_de(yaml, &expected);

    // A string that now resolves as a boolean is quoted on serialization, so
    // the round trip keeps it a string.
    let value = Value::string("yes".to_owned());
    let serialized = dbt_yaml::to_string(&value).unwrap();
    assert_eq!(serialized, "'yes'\n");
    let reparsed = dbt_yaml::from_str::<Value>(&serialized).unwrap();
    assert_eq!(reparsed, value);
}

#[test]
fn test_parse_number() {
    let n = "111".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(111));

    let n = "-111".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(-111));

    let n = "-1.1".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(-1.1));

    let n = ".nan".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(f64::NAN));
    assert!(n.as_f64().unwrap().is_sign_positive());

    let n = ".inf".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(f64::INFINITY));

    let n = "-.inf".parse::<Number>().unwrap();
    assert_eq!(n, Number::from(f64::NEG_INFINITY));

    let err = "null".parse::<Number>().unwrap_err();
    assert_eq!(err.to_string(), "failed to parse YAML number");

    let err = " 1 ".parse::<Number>().unwrap_err();
    assert_eq!(err.to_string(), "failed to parse YAML number");
}

#[test]
fn test_multiline_string() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Data {
        a: String,
        b: String,
        c: String,
        d: String,
        e: Vec<String>,
    }

    let yaml = indoc! {"
        a: |
          foo
          bar
        b: >
          foo
          bar
        c: |2
          foo
          bar
        d: '
          foo
          bar
        '
        e: 
          - foo
          - bar
    "};

    let expected = Data {
        a: "foo\nbar\n".to_owned(),
        b: "foo bar\n".to_owned(),
        c: "foo\nbar\n".to_owned(),
        d: " foo bar ".to_owned(),
        e: vec!["foo".to_owned(), "bar".to_owned()],
    };

    test_de(yaml, &expected);
}

#[cfg(feature = "yaml_11")]
mod yaml_11_timestamps {
    use dbt_yaml::{TimeOfDay, Timestamp, Value};
    use indoc::indoc;
    use serde_derive::Deserialize;
    use std::collections::BTreeMap;

    fn ts(
        year: i32,
        month: u8,
        day: u8,
        time: Option<(u8, u8, u8, u32)>,
        tz_minutes: Option<i32>,
    ) -> Value {
        Value::timestamp(Timestamp::new(
            year,
            month,
            day,
            time.map(|(h, m, s, n)| TimeOfDay::new(h, m, s, n)),
            tz_minutes,
        ))
    }

    #[test]
    fn test_timestamp_resolution() {
        // The examples from https://yaml.org/type/timestamp.html.
        let yaml = indoc! {"
            canonical: 2001-12-15T02:59:43.1Z
            valid_iso8601: 2001-12-14t21:59:43.10-05:00
            space_separated: 2001-12-14 21:59:43.10 -5
            no_time_zone: 2001-12-15 2:59:43.10
            date: 2002-12-14
        "};
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        let mapping = value.as_mapping().unwrap();

        // The first four denote the same instant; per the spec, an omitted
        // zone means UTC.
        let instant = ts(2001, 12, 15, Some((2, 59, 43, 100_000_000)), Some(0));
        for key in [
            "canonical",
            "valid_iso8601",
            "space_separated",
            "no_time_zone",
        ] {
            assert_eq!(mapping.get(key).unwrap(), &instant, "{key}");
        }
        assert_eq!(mapping.get("date").unwrap(), &ts(2002, 12, 14, None, None));
    }

    #[test]
    fn test_timestamp_grammar_details() {
        // One-digit month/day/hour in the date-time form, optional zone
        // minutes, full nanosecond fraction.
        let value = dbt_yaml::from_str::<Value>("2001-2-4 2:59:43.123456789 +5").unwrap();
        assert_eq!(
            value,
            ts(2001, 2, 4, Some((2, 59, 43, 123_456_789)), Some(300))
        );
    }

    #[test]
    fn test_timestamp_resolution_rejects() {
        // Scalars that do not match the grammar stay strings.
        for yaml in [
            "2001-2-15", // the date-only form requires two-digit month and day
            "2001-12-15 2:59",
            "2001-12",
        ] {
            let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
            match value {
                Value::String(string, ..) => assert_eq!(string, *yaml),
                _ => panic!("expected string. input={:?}, result={:?}", yaml, value),
            }
        }

        // Quoted scalars never resolve.
        let value = dbt_yaml::from_str::<Value>("\"2001-12-15\"").unwrap();
        assert_eq!(value, Value::string("2001-12-15".to_owned()));
    }

    #[test]
    fn test_timestamp_resolution_out_of_range() {
        // Components are grammar-checked only; scalars with out-of-range
        // components still resolve to timestamps, which round-trip
        // unchanged. Semantic validation is left to the consumer.
        for (yaml, canonical, timestamp) in [
            ("2001-13-01", "2001-13-01", ts(2001, 13, 1, None, None)),
            ("2001-02-29", "2001-02-29", ts(2001, 2, 29, None, None)),
            (
                "2001-12-15T24:00:00",
                "2001-12-15T24:00:00",
                ts(2001, 12, 15, Some((24, 0, 0, 0)), None),
            ),
        ] {
            let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
            assert_eq!(value, timestamp, "{yaml:?}");
            let serialized = dbt_yaml::to_string(&value).unwrap();
            assert_eq!(serialized.trim(), canonical);
            let reparsed = dbt_yaml::from_str::<Value>(&serialized).unwrap();
            assert_eq!(reparsed, value);
        }
    }

    #[test]
    fn test_timestamp_round_trip() {
        let value = dbt_yaml::from_str::<Value>("d: 2001-12-14t21:59:43.10-05:00\n").unwrap();
        let yaml = dbt_yaml::to_string(&value).unwrap();
        assert_eq!(yaml, "d: 2001-12-14T21:59:43.100-05:00\n");
        let reparsed = dbt_yaml::from_str::<Value>(&yaml).unwrap();
        assert_eq!(value, reparsed);
        assert!(reparsed["d"].is_timestamp());
    }

    #[test]
    fn test_timestamp_string_round_trip() {
        // A string that matches the timestamp grammar is quoted on
        // serialization so that it stays a string.
        let value = Value::string("2001-12-15".to_owned());
        let yaml = dbt_yaml::to_string(&value).unwrap();
        assert_eq!(yaml, "'2001-12-15'\n");
        let reparsed = dbt_yaml::from_str::<Value>(&yaml).unwrap();
        assert_eq!(reparsed, value);
    }

    #[test]
    fn test_timestamp_typed() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Config {
            created: Timestamp,
            updated: Option<Timestamp>,
            created_str: String,
            updated_str: Option<String>,
        }

        let yaml = indoc! {"
            created: 2001-12-15T02:59:43.1Z
            updated: 2002-12-14
            created_str: 2001-12-15T02:59:43.1Z
            updated_str: 2002-12-14
        "};
        let config: Config = dbt_yaml::from_str(yaml).unwrap();
        assert_eq!(
            config,
            Config {
                created: Timestamp::new(
                    2001,
                    12,
                    15,
                    Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
                    Some(0)
                ),
                updated: Some(Timestamp::new(2002, 12, 14, None, None)),
                created_str: "2001-12-15T02:59:43.1Z".to_string(),
                updated_str: Some("2002-12-14".to_string()),
            }
        );

        // From a Value, a timestamp delivers as its canonical string to
        // string-typed fields (this is what chrono's Deserialize impls
        // expect)...

        let value: Value = dbt_yaml::from_str(yaml).unwrap();
        let config: Config = value.to_typed(|_, _, _| {}, |_| Ok(None)).unwrap();
        assert_eq!(
            config,
            Config {
                created: Timestamp::new(
                    2001,
                    12,
                    15,
                    Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
                    Some(0)
                ),
                updated: Some(Timestamp::new(2002, 12, 14, None, None)),
                created_str: "2001-12-15T02:59:43.100Z".to_string(),
                updated_str: Some("2002-12-14".to_string()),
            }
        );

        let config_val: Value = value
            .clone()
            .into_typed(|_, _, _| {}, |_| Ok(None))
            .unwrap();
        assert_eq!(config_val, value);

        let value = dbt_yaml::from_str::<Value>("2001-12-15T02:59:43.1Z").unwrap();
        let string: String = dbt_yaml::from_value(value.clone()).unwrap();
        assert_eq!(string, "2001-12-15T02:59:43.100Z");
        // ...or as a Timestamp to timestamp-typed fields.
        let timestamp: Timestamp = dbt_yaml::from_value(value).unwrap();
        assert_eq!(timestamp, config.created);
    }

    #[test]
    fn test_timestamp_as_mapping_key() {
        let yaml = indoc! {"
            2001-12-15: a
            2001-12-14t21:59:43.10-05:00: b
        "};
        let map: BTreeMap<Timestamp, String> = dbt_yaml::from_str(yaml).unwrap();
        assert_eq!(map.len(), 2);

        // Keys are looked up by instant: a different spelling of the same
        // instant finds the entry.
        let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
        let mapping = value.as_mapping().unwrap();
        let probe = ts(2001, 12, 15, Some((0, 0, 0, 0)), Some(0));
        assert_eq!(mapping.get(&probe).unwrap().as_str().unwrap(), "a");
    }

    #[test]
    fn test_timestamp_duplicate_keys() {
        // Two spellings of the same instant are duplicate keys.
        let yaml = indoc! {"
            2001-12-15: a
            2001-12-15T00:00:00Z: b
        "};
        let err = dbt_yaml::from_str::<Value>(yaml).unwrap_err();
        assert!(
            err.to_string()
                .contains("duplicate entry with key 2001-12-15"),
            "{err}"
        );
    }

    #[test]
    fn test_timestamp_tuple_form() {
        // 3, 7 or 8 elements, depending on the optional time-of-day and zone.
        let time = TimeOfDay::new(2, 59, 43, 100_000_000);
        let cases = [
            ("[2001, 12, 15]", Timestamp::new(2001, 12, 15, None, None)),
            (
                "[2001, 12, 15, 2, 59, 43, 100000000]",
                Timestamp::new(2001, 12, 15, Some(time), None),
            ),
            (
                "[2001, 12, 15, 2, 59, 43, 100000000, -300]",
                Timestamp::new(2001, 12, 15, Some(time), Some(-300)),
            ),
        ];
        for (yaml, expected) in cases {
            let parsed: Timestamp = dbt_yaml::from_str(yaml).unwrap();
            assert_eq!(parsed, expected, "{yaml}");
        }

        // Too short, too long, and a zone without a time-of-day are errors.
        for yaml in [
            "[2001, 12, 15, 2]",
            "[2001, 12, 15, 2, 59]",
            "[2001, 12, 15, 2, 59, 43, 0, -300, 1]",
            "[2001, 12, 15, -300]",
        ] {
            assert!(dbt_yaml::from_str::<Timestamp>(yaml).is_err(), "{yaml}");
        }
    }

    #[test]
    fn test_timestamp_struct_form() {
        let time = TimeOfDay::new(2, 59, 43, 100_000_000);
        let cases = [
            (
                "{year: 2001, month: 12, day: 15}",
                Timestamp::new(2001, 12, 15, None, None),
            ),
            // Field order does not matter, and the time-of-day may be a
            // tuple or a struct.
            (
                "{day: 15, year: 2001, month: 12, time: [2, 59, 43, 100000000]}",
                Timestamp::new(2001, 12, 15, Some(time), None),
            ),
            (
                "{year: 2001, month: 12, day: 15, time: {hour: 2, minute: 59, second: 43, nanosecond: 100000000}, tz_minutes: -300}",
                Timestamp::new(2001, 12, 15, Some(time), Some(-300)),
            ),
            // An explicit null time-of-day is the same as omitting it.
            (
                "{year: 2001, month: 12, day: 15, time: null}",
                Timestamp::new(2001, 12, 15, None, None),
            ),
        ];
        for (yaml, expected) in cases {
            let parsed: Timestamp = dbt_yaml::from_str(yaml).unwrap();
            assert_eq!(parsed, expected, "{yaml}");
        }

        // Unknown fields are ignored; missing fields are an error.
        let parsed: Timestamp =
            dbt_yaml::from_str("{year: 2001, month: 12, day: 15, extra: 1}").unwrap();
        assert_eq!(parsed, Timestamp::new(2001, 12, 15, None, None));
        for yaml in [
            "{year: 2001, month: 12}",
            "{year: 2001, month: 12, day: 15, time: [2, 59]}",
        ] {
            assert!(dbt_yaml::from_str::<Timestamp>(yaml).is_err(), "{yaml}");
        }
    }

    #[test]
    fn test_timestamp_shapes_as_struct_field() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Config {
            created: Timestamp,
        }

        let expected = Config {
            created: Timestamp::new(2001, 12, 15, None, None),
        };
        for yaml in [
            "created: 2001-12-15",
            "created: \"2001-12-15\"",
            "created: [2001, 12, 15]",
            "created: {year: 2001, month: 12, day: 15}",
        ] {
            assert_eq!(
                dbt_yaml::from_str::<Config>(yaml).unwrap(),
                expected,
                "{yaml}"
            );
        }
    }

    #[test]
    fn test_timestamp_type_survives_value_conversion() {
        let timestamp = Timestamp::new(
            2001,
            12,
            15,
            Some(TimeOfDay::new(2, 59, 43, 100_000_000)),
            Some(-300),
        );

        // Serializing to a Value keeps the Timestamp type instead of
        // flattening it to a string.
        let value = dbt_yaml::to_value(timestamp).unwrap();
        assert!(value.is_timestamp());
        assert_eq!(value, Value::timestamp(timestamp));

        // Deserializing a Timestamp Value keeps the Timestamp type too...
        let parsed: Timestamp = dbt_yaml::from_value(value.clone()).unwrap();
        assert_eq!(parsed, timestamp);
        // ...while string-typed targets still receive the canonical form.
        let string: String = dbt_yaml::from_value(value).unwrap();
        assert_eq!(string, "2001-12-15T02:59:43.100-05:00");

        // A zone without a time-of-day has no place in the component payload
        // and, like Display, does not survive serialization.
        let zone_only = Timestamp::new(2001, 12, 15, None, Some(-300));
        let value = dbt_yaml::to_value(zone_only).unwrap();
        assert_eq!(
            value.as_timestamp().unwrap(),
            &Timestamp::new(2001, 12, 15, None, None)
        );
    }

    #[test]
    fn test_timestamp_span() {
        // A resolved timestamp keeps the span of its scalar, exactly like a
        // string scalar.
        let value = dbt_yaml::from_str::<Value>("d: 2001-12-15\ns: 2001-12-15\n").unwrap();
        let timestamp = value["d"].span();
        assert_eq!(timestamp.start.line, 1);
        assert_eq!(timestamp.start.index, 3);
        assert_eq!(timestamp.end.index, 14);

        let string = value["s"].span();
        assert_eq!(
            string.end.index - string.start.index,
            timestamp.end.index - timestamp.start.index
        );

        let sequence = dbt_yaml::from_str::<Value>("- 2001-12-15\n").unwrap();
        assert_eq!(sequence[0].span().start.index, 2);
    }
}
