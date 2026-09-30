#![allow(
    clippy::derive_partial_eq_without_eq,
    clippy::eq_op,
    clippy::uninlined_format_args
)]

use std::collections::HashMap;

use dbt_yaml::Spanned;
use dbt_yaml::{value::TransformedResult, Number, Value, Verbatim};
use dbt_yaml_derive::UntaggedEnumDeserialize;
use indoc::indoc;
use serde::de::{DeserializeOwned, IntoDeserializer};
use serde::Deserialize as _;
use serde_derive::{Deserialize, Serialize};

#[test]
fn test_nan() {
    let pos_nan = dbt_yaml::from_str::<Value>(".nan").unwrap();
    assert!(pos_nan.is_f64());
    assert_eq!(pos_nan, pos_nan);

    let neg_fake_nan = dbt_yaml::from_str::<Value>("-.nan").unwrap();
    assert!(neg_fake_nan.is_string());

    let significand_mask = 0xF_FFFF_FFFF_FFFF;
    let bits = (f64::NAN.copysign(1.0).to_bits() ^ significand_mask) | 1;
    let different_pos_nan = Value::number(Number::from(f64::from_bits(bits)));
    assert_eq!(pos_nan, different_pos_nan);
}

#[test]
fn test_digits() {
    let num_string = dbt_yaml::from_str::<Value>("01").unwrap();
    assert!(num_string.is_string());
}

#[test]
fn test_into_deserializer() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Test {
        first: String,
        second: u32,
    }

    let value = dbt_yaml::from_str::<Value>("xyz").unwrap();
    let s = String::deserialize(value.into_deserializer()).unwrap();
    assert_eq!(s, "xyz");

    let value = dbt_yaml::from_str::<Value>("- first\n- second\n- third").unwrap();
    let arr = Vec::<String>::deserialize(value.into_deserializer()).unwrap();
    assert_eq!(arr, &["first", "second", "third"]);

    let value = dbt_yaml::from_str::<Value>("first: abc\nsecond: 99").unwrap();
    let test = Test::deserialize(value.into_deserializer()).unwrap();
    assert_eq!(
        test,
        Test {
            first: "abc".to_string(),
            second: 99
        }
    );
}

type UnusedKeys = Vec<(String, Value, Value)>;

fn deserialize_value_inner<T: DeserializeOwned + PartialEq + std::fmt::Debug>(
    value: Value,
    mut transformer: impl Fn(&Value) -> TransformedResult,
) -> (Result<T, dbt_yaml::Error>, UnusedKeys) {
    let mut unused_keys1 = vec![];
    let mut unused_keys2 = vec![];

    let borrowed_path: Result<T, dbt_yaml::Error> = value.to_typed(
        |path, key: &Value, value: &Value| {
            unused_keys2.push((path.to_string(), key.clone(), value.clone()));
        },
        &mut transformer,
    );
    let owned_path: Result<T, dbt_yaml::Error> = value.into_typed(
        |path, key: &Value, value: &Value| {
            unused_keys1.push((path.to_string(), key.clone(), value.clone()));
        },
        &mut transformer,
    );

    match (&borrowed_path, &owned_path) {
        (Ok(borrowed), Ok(owned)) => {
            assert_eq!(borrowed, owned);
        }
        (Err(e1), Err(e2)) => {
            assert_eq!(e1.to_string(), e2.to_string());
        }
        _ => {
            panic!(
                "Mismatched results: {:?} vs {:?}",
                borrowed_path, owned_path
            );
        }
    }

    assert!(unused_keys1 == unused_keys2);

    (owned_path, unused_keys1)
}

fn deserialize_value<T: DeserializeOwned + PartialEq + std::fmt::Debug>(
    value: Value,
    transformer: impl Fn(&Value) -> TransformedResult,
) -> (T, UnusedKeys) {
    let (result, unused_keys) = deserialize_value_inner(value, transformer);
    (result.unwrap(), unused_keys)
}

#[test]
fn test_into_typed() {
    fn transformer(v: &Value) -> Result<Option<Value>, Box<dyn std::error::Error + Send + Sync>> {
        match v {
            Value::String(s, span) => Ok(Some(Value::String(format!("{} name", s), span.clone()))),
            _ => Ok(None),
        }
    }

    let value = dbt_yaml::from_str::<Value>("xyz").unwrap();
    let (s, unused_keys): (String, _) = deserialize_value(value, |_| Ok(None));
    assert!(unused_keys.is_empty());
    assert_eq!(s, "xyz");

    let value = dbt_yaml::from_str::<Value>("- first\n- second\n- third").unwrap();
    let (arr, unused_keys): (Vec<String>, _) = deserialize_value(value, transformer);
    assert!(unused_keys.is_empty());
    assert_eq!(arr, &["first name", "second name", "third name"]);

    #[derive(Debug, Deserialize, PartialEq)]
    struct Test {
        first: String,
        second: u32,
    }
    #[derive(Debug, Deserialize, PartialEq)]
    struct Test2 {
        first: Verbatim<Value>,
        third: u32,
        fourth: Value,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        first: abc
        second: 99
        third: 100
        fourth: my
        "})
    .unwrap();

    let (test, unused_keys): (Test, _) = deserialize_value(value.clone(), transformer);
    assert_eq!(
        unused_keys,
        vec![
            (
                "third".to_string(),
                Value::string("third".to_string()),
                Value::number(Number::from(100))
            ),
            (
                "fourth".to_string(),
                Value::string("fourth".to_string()),
                Value::string("my".to_string())
            )
        ]
    );
    assert_eq!(
        test,
        Test {
            first: "abc name".to_string(),
            second: 99
        }
    );

    let (test2, unused_keys): (Test2, _) = deserialize_value(value, transformer);
    assert_eq!(
        unused_keys,
        vec![(
            "second".to_string(),
            Value::string("second".to_string()),
            Value::number(Number::from(99))
        )]
    );
    assert_eq!(
        test2,
        Test2 {
            // field_transform is not applied to `Verbatim`-typed fields:
            first: Value::string("abc".to_string()).into(),
            third: 100,
            // Non-verbatim fields should be transformed:
            fourth: Value::string("my name".to_string()),
        }
    );

    #[derive(Debug, Deserialize, PartialEq)]
    struct Test3 {
        first: Test,
        seconds: Vec<Verbatim<Value>>,
        third: Option<Value>,
        fourth: Option<Option<String>>,
        #[serde(flatten)]
        rest: Option<HashMap<String, Value>>,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        first:
          first: abc
          second: 99
          third: 100
        seconds:
          -   first: A
              second: 1
              third: 1
              fourth: D
          -   first: B
              third: 2
              fourth: X
        fourth: xyz
        third: xyz
        fifth:
          sixth: cde
        "});
    let (test3, unused_keys): (Test3, _) = deserialize_value(value.unwrap(), transformer);

    assert_eq!(
        unused_keys,
        vec![(
            "first.third".to_string(),
            Value::string("third".to_string()),
            Value::number(Number::from(100))
        )]
    );

    assert_eq!(
        test3.first,
        Test {
            first: "abc name".to_string(),
            second: 99
        }
    );
    assert_eq!(test3.third, Some(Value::string("xyz name".to_string())));
    assert_eq!(test3.fourth, Some(Some("xyz name".to_string())));
    assert_eq!(
        test3.rest,
        Some(HashMap::from([(
            "fifth".to_string(),
            Value::mapping(
                [(
                    Value::string("sixth".to_string()),
                    Value::string("cde name".to_string()),
                )]
                .into_iter()
                .collect()
            )
        )]))
    );
    assert_eq!(test3.seconds.len(), 2);
    let (test2_1, unused_keys): (Test2, _) =
        deserialize_value(test3.seconds[0].as_ref().clone(), |v| {
            if let Some(n) = v.as_u64() {
                Ok(Some(Value::number(Number::from(n + 2))))
            } else {
                Ok(None)
            }
        });
    assert_eq!(
        unused_keys,
        vec![(
            "second".to_string(),
            Value::string("second".to_string()),
            Value::number(Number::from(1))
        )],
    );
    assert_eq!(
        test2_1,
        Test2 {
            first: Value::string("A".to_string()).into(),
            third: 3,
            fourth: Value::string("D".to_string()),
        }
    );
}

#[test]
fn test_value_to_value_with_transformer() {
    // Value -> Value deserialization with an active field transformer must
    // still visit nested values: a transformer that leaves a container
    // unchanged may still transform its contents.
    fn transformer(v: &Value) -> Result<Option<Value>, Box<dyn std::error::Error + Send + Sync>> {
        if v.is_u64() {
            Ok(Some(Value::from(v.as_u64().unwrap() + 100)))
        } else if v.is_bool() {
            Ok(Some(Value::from(true)))
        } else {
            Ok(None)
        }
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        a: 1
        b:
          - 2
          - c: false
        "})
    .unwrap();
    let expected = dbt_yaml::from_str::<Value>(indoc! {"
        a: 101
        b:
          - 102
          - c: true
        "})
    .unwrap();

    let (transformed, unused_keys): (Value, _) = deserialize_value(value, transformer);
    assert!(unused_keys.is_empty());
    assert_eq!(transformed, expected);
}

#[test]
fn test_value_to_value_with_transformer_tagged() {
    // FIXME: the field transformer is not applied to a tag's contents:
    // `TaggedValue`'s `EnumAccess` impl deserializes them without the
    // transformer.
    let value = dbt_yaml::from_str::<Value>("a: !some_tag 1").unwrap();
    let (round_tripped, unused_keys): (Value, _) = deserialize_value(value.clone(), |_| Ok(None));
    assert!(unused_keys.is_empty());
    assert_eq!(round_tripped, value);

    fn tag_transformer(
        v: &Value,
    ) -> Result<Option<Value>, Box<dyn std::error::Error + Send + Sync>> {
        match v {
            Value::Tagged(..) => Ok(Some(Value::from("replaced"))),
            _ => Ok(None),
        }
    }
    let value = dbt_yaml::from_str::<Value>("a: !some_tag 1").unwrap();
    let (transformed, unused_keys): (Value, _) = deserialize_value(value, tag_transformer);
    assert!(unused_keys.is_empty());
    assert_eq!(
        transformed,
        dbt_yaml::from_str::<Value>("a: replaced").unwrap()
    );
}

#[test]
fn test_into_typed_external_err() {
    #[derive(Debug, PartialEq)]
    struct Error {
        msg: String,
    }
    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Error: {}", self.msg)
        }
    }
    impl std::error::Error for Error {}

    let value = dbt_yaml::from_str::<Value>("xyz").unwrap();
    let (err, unused_keys) = deserialize_value_inner::<String>(value, |v| {
        Err(Error {
            msg: format!("error {}", v.as_str().unwrap()),
        }
        .into())
    });
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(
        err.unwrap_err()
            .into_external()
            .unwrap()
            .downcast::<Error>()
            .unwrap(),
        Box::new(Error {
            msg: "error xyz".to_string()
        })
    );
}

#[test]
fn test_merge() {
    // From https://yaml.org/type/merge.html.
    let yaml = indoc! {"
        ---
        - &CENTER { x: 1, y: 2 }
        - &LEFT { x: 0, y: 2 }
        - &BIG { r: 10 }
        - &SMALL { r: 1 }
        - &RECURSE { <<: *CENTER }

        # All the following maps are equal:

        - # Explicit keys
          x: 1
          y: 2
          r: 10
          label: center/big

        - # Merge one map
          << : *CENTER
          r: 10
          label: center/big

        - # Merge multiple maps
          << : [ *CENTER, *BIG ]
          label: center/big

        - # Override
          << : [ *BIG, *LEFT, *SMALL ]
          x: 1
          label: center/big

        - # Merge with reference that also contains a merge
          << : *RECURSE
          r: 10
          label: center/big
    "};

    let mut value: Value = dbt_yaml::from_str(yaml).unwrap();
    value.apply_merge().unwrap();
    assert!(value.span().is_valid());
    for i in 6..=9 {
        assert_eq!(value[5], value[i]);
    }
}

#[test]
fn test_debug() {
    let yaml = indoc! {"
        'Null': ~
        Bool: true
        Number: 1
        String: ...
        Sequence:
          - true
        EmptySequence: []
        EmptyMapping: {}
        Tagged: !tag true
    "};

    let value: Value = dbt_yaml::from_str(yaml).unwrap();
    assert!(value.span().is_valid());
    let debug = format!("{:#?}", value);
    eprintln!("{debug}");

    let expected = indoc! {r#"
Mapping {
    String("Null") @{1:1[0]..1:9[8]}: Null @{1:9[8]..2:1[10]},
    String("Bool") @{2:1[10]..2:7[16]}: Bool(true) @{2:7[16]..3:1[21]},
    String("Number") @{3:1[21]..3:9[29]}: Number(1) @{3:9[29]..4:1[31]},
    String("String") @{4:1[31]..4:9[39]}: String("...") @{4:9[39]..5:1[43]},
    String("Sequence") @{5:1[43]..6:3[55]}: Sequence [
        Bool(true) @{6:5[57]..7:1[62]},
    ] @{6:3[55]..7:1[62]},
    String("EmptySequence") @{7:1[62]..7:16[77]}: Sequence [] @{7:16[77]..8:1[80]},
    String("EmptyMapping") @{8:1[80]..8:15[94]}: Mapping {} @{8:15[94]..9:1[97]},
    String("Tagged") @{9:1[97]..9:9[105]}: TaggedValue {
        tag: !tag,
        value: Bool(true) @{10:1[115]..10:1[115]},
    } @{9:9[105]..10:1[115]},
} @{1:1[0]..10:1[115]}"#
    };

    assert_eq!(debug, expected);
}

#[test]
fn test_tagged() {
    #[derive(Serialize)]
    enum Enum {
        Variant(usize),
    }

    let value = dbt_yaml::to_value(Enum::Variant(0)).unwrap();

    let deserialized: dbt_yaml::Value = dbt_yaml::from_value(value.clone()).unwrap();
    assert_eq!(value, deserialized);

    let serialized = dbt_yaml::to_value(&value).unwrap();
    assert_eq!(value, serialized);
}

#[test]
fn test_value_span() {
    let yaml = "x: 1.0\ny: 2.0\n";
    let value: Value = dbt_yaml::from_str(yaml).unwrap();
    assert!(value.span().is_valid());
    assert_eq!(value.span().start.index, 0);
    assert_eq!(value.span().start.line, 1);
    assert_eq!(value.span().start.column, 1);
    assert_eq!(value.span().end.index, 14);
    assert_eq!(value.span().end.line, 3);
    assert_eq!(value.span().end.column, 1);

    match value {
        Value::Mapping(map, ..) => {
            let v = map.get(Value::string("x".to_string())).unwrap();
            assert!(v.span().is_valid());
            assert_eq!(v.span().start.line, 1);
            assert_eq!(v.span().start.column, 4);
            assert_eq!(v.span().end.line, 2);
            assert_eq!(v.span().end.column, 1);
            assert_eq!(yaml[v.span().start.index..v.span().end.index].trim(), "1.0");

            let keys = map.keys().collect::<Vec<_>>();
            assert_eq!(keys.len(), 2);
            let x = keys[0];
            assert!(x.span().is_valid());
            assert_eq!(x.span().start.line, 1);
            assert_eq!(x.span().start.column, 1);
            assert_eq!(x.span().end.line, 1);
            assert_eq!(yaml[x.span().start.index..x.span().end.index].trim(), "x:");

            let y = keys[1];
            assert!(y.span().is_valid());
            assert_eq!(y.span().start.line, 2);
            assert_eq!(y.span().start.column, 1);
            assert_eq!(y.span().end.line, 2);
            assert_eq!(yaml[y.span().start.index..y.span().end.index].trim(), "y:");
        }
        _ => panic!("expected mapping"),
    }
}

#[test]
fn test_value_span_multidoc() {
    let yaml = indoc! {"
        ---
        x: 1.0
        y: 2.0
        ---
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
        ---
    "};
    let mut values = vec![];
    for document in dbt_yaml::Deserializer::from_str(yaml) {
        let value = Value::deserialize(document).unwrap();
        values.push(value);
    }
    assert_eq!(values.len(), 3);
    assert!(values[0].span().is_valid());
    assert!(values[1].span().is_valid());
    assert_eq!(
        yaml[values[0].span().start.index..values[0].span().end.index].trim(),
        "x: 1.0\ny: 2.0"
    );

    assert_eq!(values[1].span().start.line, 5);
    assert_eq!(values[1].span().start.column, 1);

    let struc_key_span = values[1]
        .as_mapping()
        .unwrap()
        .keys()
        .next()
        .unwrap()
        .span();
    assert_eq!(struc_key_span.start.line, 5);
    assert_eq!(struc_key_span.start.column, 1);
    assert_eq!(struc_key_span.end.line, 5);
    assert_eq!(struc_key_span.end.column, 8);

    let tuple_span = values[1].get("tuple").unwrap().span();
    assert_eq!(
        yaml[tuple_span.start.index..tuple_span.end.index].trim(),
        "!wat\n  - 0\n  - 0"
    );
}

#[test]
fn test_verbatim() {
    let yaml = indoc! {"
        x: 1
        y: 2
        z: 3
    "};

    #[derive(Deserialize, PartialEq, Eq, Debug, Hash)]
    struct Thing {
        x: i32,
        y: Verbatim<i32>,
        z: Verbatim<Option<i32>>,
        v: Verbatim<Option<String>>,
    }

    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let thing: Thing = value
        .into_typed(
            |path, key: &Value, _| {
                panic!("unexpected key {:?} at path {:?}", key, path.to_string());
            },
            |v| {
                if let Some(v) = v.as_i64() {
                    Ok(Some(Value::from(v + 100)))
                } else {
                    Ok(None)
                }
            },
        )
        .unwrap();

    assert_eq!(thing.x, 101);
    assert_eq!(*thing.y, 2);
    assert_eq!(*thing.z, Some(3));
    assert!(thing.v.is_none());

    let thing2: Thing = dbt_yaml::from_str(indoc! {"
        x: 101
        y: 2
        z: 3
    "})
    .unwrap();
    assert_eq!(thing, thing2);
}

#[test]
fn test_verbatim_flatten() {
    #[derive(Deserialize, Serialize, PartialEq, Eq, Debug)]
    struct Thing2 {
        x: Option<i32>,
        y: Verbatim<i32>,
        #[serde(flatten)]
        rest: Verbatim<HashMap<String, Option<i32>>>,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        x: 1
        y: 2
        z: 3
    "})
    .unwrap();
    let thing2: Thing2 = value
        .into_typed(
            |path, key: &Value, _| {
                panic!("unexpected key {:?} at path {:?}", key, path.to_string());
            },
            |v| {
                if v.is_i64() {
                    Ok(Some(Value::null()))
                } else {
                    Ok(None)
                }
            },
        )
        .unwrap();
    assert_eq!(thing2.x, None);
    assert_eq!(*thing2.y, 2);
    // Note: unfortunately `Verbatim` does not work in `flatten` fields:
    assert_eq!(*thing2.rest, HashMap::from_iter([("z".to_string(), None)]));

    let value = dbt_yaml::to_value(thing2).unwrap();
    assert_eq!(
        value,
        dbt_yaml::from_str::<Value>(indoc! {"
            x: null
            y: 2
            z: null
        "})
        .unwrap()
    );
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_flatten() {
    #[derive(Deserialize, Serialize, PartialEq, Eq, Debug)]
    struct Thing3 {
        x: Option<i32>,
        y: Verbatim<i32>,
        __flatten__: HashMap<String, Verbatim<Option<i32>>>,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        x: 1
        y: 2
        z: 3
    "})
    .unwrap();
    let (thing3, unused_keys) = deserialize_value::<Thing3>(value, |_| Ok(None));
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing3.x, Some(1));
    assert_eq!(*thing3.y, 2);
    assert_eq!(*thing3.__flatten__["z"], Some(3));

    let value = dbt_yaml::to_value(thing3).unwrap();
    assert_eq!(
        value,
        dbt_yaml::from_str::<Value>(indoc! {"
            x: 1
            y: 2
            z: 3
        "})
        .unwrap()
    );

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        y: 2
    "})
    .unwrap();
    let thing3 = Thing3::deserialize(value.into_deserializer()).unwrap();
    assert_eq!(
        thing3,
        Thing3 {
            x: None,
            y: 2.into(),
            __flatten__: HashMap::new()
        }
    );
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_verbatim_flatten_nested() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing4 {
        x: Verbatim<Option<i32>>,
        __thing5__: Thing5,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing5 {
        a: Verbatim<Option<i32>>,
        __rest__: HashMap<String, Thing4>,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        a: 3
        x: 1
        b:
          x: 2
    "})
    .unwrap();
    let (thing4, unused_keys) = deserialize_value::<Thing4>(value, |v| {
        if v.is_i64() {
            panic!("transformer should not be called for i64 values: {:?}", v);
        } else {
            Ok(None)
        }
    });
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(*thing4.x, Some(1));
    assert_eq!(*thing4.__thing5__.a, Some(3));
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_flatten_untagged_enum_in_first_slot() {
    // Regression test: an UntaggedEnumDeserialize enum in a non-last
    // flatten_dunder slot (served by FlattenDeserializer) used to segfault:
    // the deserializer state saved a borrowed pointer to a stack-local closure
    // that the derive invoked after the call had returned.
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct RangeConfig {
        range: i32,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct TimeConfig {
        granularity: String,
    }

    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    #[serde(untagged)]
    enum Partition {
        Range(RangeConfig),
        Time(TimeConfig),
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Thing {
        field: String,
        __inner__: Partition,
        __rest__: HashMap<String, Verbatim<Option<i32>>>,
    }

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        range: 3
        extra: 42
    "})
    .unwrap();
    let (thing, unused_keys) = deserialize_value::<Thing>(value, |_| Ok(None));
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing.field, "c1");
    assert_eq!(thing.__inner__, Partition::Range(RangeConfig { range: 3 }));
    // Keys the matched variant did not consume flow to the next flatten field.
    assert_eq!(*thing.__rest__["extra"], Some(42));

    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        granularity: day
    "})
    .unwrap();
    let (thing, _) = deserialize_value::<Thing>(value, |_| Ok(None));
    assert_eq!(
        thing.__inner__,
        Partition::Time(TimeConfig {
            granularity: "day".to_string()
        })
    );
    assert!(thing.__rest__.is_empty());

    // No matching variant: the saved callback is dropped without invocation.
    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
    "})
    .unwrap();
    let (result, _) = deserialize_value_inner::<Thing>(value, |_| Ok(None));
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("data did not match any variant of untagged enum Partition"),
        "unexpected error: {err}"
    );
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_flatten_mixed_enum_in_first_slot() {
    // A mixed enum (tagged variants + untagged fallback) in a non-last
    // flatten_dunder slot: dispatch must work through the flatten machinery
    // and genuinely unused keys must flow to the next flatten field.
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct RangeConfig {
        range: i32,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct TimeConfig {
        granularity: String,
    }

    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    #[serde(tag = "kind")]
    enum Partition {
        Range(RangeConfig),
        #[serde(untagged)]
        Time(TimeConfig),
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Thing {
        field: String,
        __inner__: Partition,
        __rest__: HashMap<String, Verbatim<Option<i32>>>,
    }

    // Tagged match: the tag and variant fields are consumed; the extra key
    // flows through to __rest__.
    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        kind: Range
        range: 3
        extra: 42
    "})
    .unwrap();
    let (thing, unused_keys) = deserialize_value::<Thing>(value, |_| Ok(None));
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing.__inner__, Partition::Range(RangeConfig { range: 3 }));
    assert_eq!(*thing.__rest__["extra"], Some(42));

    // Fallback: the tag is unknown, so the untagged variant matches. The tag
    // key is consumed by dispatch; the extra key flows through.
    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        kind: Nope
        granularity: day
        extra: 42
    "})
    .unwrap();
    let (thing, _) = deserialize_value::<Thing>(value, |_| Ok(None));
    assert_eq!(
        thing.__inner__,
        Partition::Time(TimeConfig {
            granularity: "day".to_string()
        })
    );
    assert_eq!(*thing.__rest__["extra"], Some(42));
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_flatten_mixed_external_enum_in_first_slot() {
    // Same as above, but with an externally-tagged mixed enum: the tag is
    // looked up among the leftover keys by variant name, matching serde's
    // FlatMapDeserializer.
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct RangeConfig {
        range: i32,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct TimeConfig {
        granularity: String,
    }

    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    enum ExtPartition {
        Range(RangeConfig),
        #[serde(untagged)]
        Time(TimeConfig),
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Thing {
        field: String,
        __ext__: ExtPartition,
        __rest__: HashMap<String, Verbatim<Option<i32>>>,
    }

    // Wrapped match: the Range key is found among the leftover keys; the
    // extra key flows through to __rest__.
    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        Range:
          range: 7
        extra: 42
    "})
    .unwrap();
    let (thing, unused_keys) = deserialize_value::<Thing>(value, |_| Ok(None));
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing.__ext__, ExtPartition::Range(RangeConfig { range: 7 }));
    assert_eq!(*thing.__rest__["extra"], Some(42));

    // Fallback: no leftover key names a variant, so the untagged variant
    // matches against the whole leftover map.
    let value = dbt_yaml::from_str::<Value>(indoc! {"
        field: c1
        granularity: day
        extra: 42
    "})
    .unwrap();
    let (thing, _) = deserialize_value::<Thing>(value, |_| Ok(None));
    assert_eq!(
        thing.__ext__,
        ExtPartition::Time(TimeConfig {
            granularity: "day".to_string()
        })
    );
    assert_eq!(*thing.__rest__["extra"], Some(42));
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_multi_flatten_fields() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing6 {
        x: Option<i32>,
        __thing7__: Thing7,
        __rest__: HashMap<String, Option<i32>>,
        y: i32,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing7 {
        a: Option<i32>,
        __thing8__: Thing8,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing8 {
        b: Option<i32>,
    }

    let yaml = indoc! {"
        a: 3
        y: 5
        b: 4
        c: 1
    "};
    let (thing6, unused_keys) =
        deserialize_value::<Thing6>(dbt_yaml::from_str::<Value>(yaml).unwrap(), |_| Ok(None));
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing6.x, None);
    assert_eq!(thing6.__thing7__.a, Some(3));
    assert_eq!(thing6.__thing7__.__thing8__.b, Some(4));
    assert_eq!(thing6.__rest__, HashMap::from([("c".to_string(), Some(1))]));

    let yaml = indoc! {"
        a: 3
        x: 1
        b: 4
    "};
    let expected_err = dbt_yaml::from_value::<Thing6>(dbt_yaml::from_str::<Value>(yaml).unwrap())
        .unwrap_err()
        .to_string();
    assert_eq!(expected_err, "missing field `y` at line 1 column 1");
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_multi_flatten_shouldbe() {
    use dbt_yaml::ShouldBe;

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing6 {
        x: Option<i32>,
        __thing7__: Thing7,
        __rest__: HashMap<String, ShouldBe<Thing6>>,
        y: Verbatim<String>,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing7 {
        a: Option<i32>,
        __thing8__: Thing8,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing8 {
        c: Option<i32>,
    }

    let yaml = indoc! {"
        a: 3
        y: '5'
        b:
          x: 4
          y: 'a string'
        bb: 1
    "};
    let (thing6, unused_keys) =
        deserialize_value::<Thing6>(dbt_yaml::from_str::<Value>(yaml).unwrap(), |v| {
            if v.is_string() {
                panic!(
                    "transformer should not be called for string values: {:?}",
                    v
                );
            } else {
                Ok(None)
            }
        });
    if !unused_keys.is_empty() {
        panic!("unexpected unused keys: {:?}", unused_keys);
    }
    assert_eq!(thing6.x, None);
    assert_eq!(thing6.__thing7__.a, Some(3));
    assert_eq!(thing6.__thing7__.__thing8__.c, None);
    assert_eq!(thing6.__rest__.len(), 2);
    assert!(thing6.__rest__["bb"].isnt());
    assert!(thing6.__rest__["b"].is());
    let inner = thing6.__rest__["b"].as_ref().unwrap();
    assert_eq!(inner.x, Some(4));
    assert_eq!(*inner.y, "a string");

    let yaml = indoc! {"
        a: 3
        x: 1
        b: 4
    "};
    let expected_err = dbt_yaml::from_value::<Thing6>(dbt_yaml::from_str::<Value>(yaml).unwrap())
        .unwrap_err()
        .to_string();
    assert_eq!(expected_err, "missing field `y` at line 1 column 1");
}

#[cfg(feature = "schemars")]
#[test]
fn test_schemars() {
    #![allow(dead_code)]

    use dbt_yaml::JsonSchema;
    use schemars::schema_for;

    #[derive(JsonSchema)]
    struct Thing {
        x: Option<Option<i32>>,
        y: Verbatim<Value, String>,
        z: Verbatim<Option<Value>>,
        v: Verbatim<Option<Option<String>>>,
    }

    let schema = schema_for!(Thing);
    let schema_string = dbt_yaml::to_string(&schema).unwrap();
    println!("{}", schema_string);
    assert_eq!(
        schema_string,
        indoc! {"
$schema: http://json-schema.org/draft-07/schema#
title: Thing
type: object
required:
- y
properties:
  v:
    type:
    - string
    - 'null'
  x:
    type:
    - integer
    - 'null'
    format: int32
  y:
    type: string
  z:
    anyOf:
    - $ref: '#/definitions/AnyValue'
    - type: 'null'
definitions:
  AnyValue: true
"}
    );
}

#[cfg(all(feature = "schemars", feature = "flatten_dunder"))]
#[test]
fn test_schemars_flatten() {
    #![allow(dead_code)]

    use dbt_yaml::JsonSchema;
    use schemars::schema_for;

    #[derive(Deserialize, JsonSchema)]
    struct Thing {
        a: i32,
        #[serde(flatten)]
        rest: Verbatim<HashMap<String, Option<i32>>>,
    }

    #[derive(Deserialize, JsonSchema)]
    struct Thing2 {
        x: Option<i32>,
        y: Verbatim<Value, i32>,
        //        #[serde(flatten)]
        __thing__: Option<Thing>,
    }

    let schema = schema_for!(Thing2);
    let schema_string = dbt_yaml::to_string(&schema).unwrap();
    println!("{}", schema_string);
    assert_eq!(
        schema_string,
        indoc! {"
$schema: http://json-schema.org/draft-07/schema#
title: Thing2
type: object
required:
- y
properties:
  a:
    type: integer
    format: int32
  x:
    type:
    - integer
    - 'null'
    format: int32
  y:
    type: integer
    format: int32
"}
    );
}

#[test]
fn test_untagged_enum() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing {
        a: Verbatim<i32>,
        b: Verbatim<Option<i32>>,
        c: bool,
    }

    #[allow(clippy::large_enum_variant)]
    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    #[serde(untagged)]
    enum Untagged<T> {
        String(String),
        Number(i32, i32),
        Thing(T),
        Unit,
    }

    let value = dbt_yaml::from_str::<Value>("[42, 32]").unwrap();
    let untagged = deserialize_value::<Untagged<String>>(value, |_| Ok(None)).0;
    assert_eq!(untagged, Untagged::Number(42, 32));

    let value = dbt_yaml::from_str::<Value>("'hello'").unwrap();
    let untagged = deserialize_value::<Untagged<i64>>(value, |_| Ok(None)).0;
    assert_eq!(untagged, Untagged::String("hello".to_string()));

    let value = dbt_yaml::from_str::<Value>("").unwrap();
    let untagged = deserialize_value::<Untagged<Thing>>(value, |_| Ok(None)).0;
    assert_eq!(untagged, Untagged::Unit);

    let yaml = indoc! {"
        a: 3
        y: '5'
        c: false
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let (untagged, unused_keys) = deserialize_value::<Untagged<Thing>>(value, |v| {
        if v.is_u64() {
            Ok(Some(Value::from(v.as_u64().unwrap() + 100)))
        } else if v.is_bool() {
            Ok(Some(Value::from(true)))
        } else {
            Ok(None)
        }
    });
    assert_eq!(
        unused_keys,
        vec![(
            "y".to_string(),
            Value::string("y".to_string()),
            Value::string("5".to_string())
        )]
    );
    assert_eq!(
        untagged,
        Untagged::Thing(Thing {
            a: 3.into(),
            b: None.into(),
            c: true,
        })
    );

    let yaml = indoc! {"
        a: 3
        b: 4
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let expected_err = Untagged::<Thing>::deserialize(value.into_deserializer())
        .unwrap_err()
        .to_string();
    assert_eq!(
        expected_err,
        "data did not match any variant of untagged enum Untagged"
    );

    #[derive(Deserialize)]
    struct ThingWithSpanned {
        v: Spanned<Vec<Spanned<i32>>>,
    }

    let yaml = indoc! {"
        v:
          - 111
          - 222
    "};

    let value: dbt_yaml::Value = dbt_yaml::from_str(yaml).unwrap();
    let thing = match Untagged::<ThingWithSpanned>::deserialize(value.into_deserializer()).unwrap()
    {
        Untagged::Thing(thing) => thing,
        _ => panic!("expected Thing variant"),
    };
    assert_eq!(thing.v.len(), 2);
    assert_eq!(thing.v.span().start.line, 2);
    assert_eq!(thing.v.span().end.line, 4);
    assert_eq!(thing.v[0].span().start.index, 7);
    assert_eq!(thing.v[0].span().end.index, 15);
    assert_eq!(thing.v[0].span().start.line, 2);
    assert_eq!(thing.v[0].span().end.line, 3);
    assert_eq!(thing.v[1].span().start.line, 3);
    assert_eq!(thing.v[1].span().end.line, 4);
}

#[test]
fn test_tagged_enum() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing {
        a: Verbatim<i32>,
        b: Verbatim<Option<i32>>,
        c: bool,
    }

    #[allow(clippy::large_enum_variant)]
    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    #[serde(tag = "type")]
    #[serde(rename_all = "snake_case")]
    enum Tagged {
        String(String),
        T(Thing),
        Unit,
    }

    let yaml = indoc! {"
        type: unit
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let tagged = deserialize_value::<Tagged>(value, |_| Ok(None)).0;
    assert_eq!(tagged, Tagged::Unit);

    let yaml = indoc! {"
        type: t
        a: 1
        c: false
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let tagged = deserialize_value::<Tagged>(value, |_| Ok(None)).0;
    assert_eq!(
        tagged,
        Tagged::T(Thing {
            a: 1.into(),
            b: None.into(),
            c: false
        })
    );

    let yaml = indoc! {"
        type: t
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let tagged = deserialize_value::<Tagged>(value, |v| {
        if v.as_str() == Some("t") {
            Ok(Some(Value::string("unit".to_string())))
        } else {
            Ok(None)
        }
    })
    .0;
    assert_eq!(tagged, Tagged::Unit);
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_untagged_enum_flatten_dunder() {
    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing {
        a: Verbatim<i32>,
        b: Verbatim<Option<i32>>,
        c: bool,
    }

    #[allow(clippy::large_enum_variant)]
    #[derive(UntaggedEnumDeserialize, PartialEq, Eq, Debug)]
    #[serde(untagged)]
    enum Untagged<T> {
        String(String),
        Number(i32, i32),
        Thing(T),
        Unit,
    }

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Thing2 {
        __untagged__: Untagged<Thing>,
        __rest__: HashMap<String, String>,
    }

    let yaml = indoc! {"
        a: 3
        c: false
        y: '5'
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let (untagged, unused_keys) = deserialize_value::<Untagged<Thing2>>(value, |v| {
        if v.is_u64() {
            Ok(Some(Value::from(v.as_u64().unwrap() + 100)))
        } else if v.is_bool() {
            Ok(Some(Value::from(true)))
        } else {
            Ok(None)
        }
    });
    assert!(unused_keys.is_empty());
    assert_eq!(
        untagged,
        Untagged::Thing(Thing2 {
            __untagged__: Untagged::Thing(Thing {
                a: 3.into(),
                b: None.into(),
                c: true,
            }),
            // Keys the matched variant did not consume flow to `__rest__`.
            __rest__: HashMap::from([("y".to_string(), "5".to_string())]),
        })
    );

    let yaml = indoc! {"
        - a: 3
          c: false
        -
        - [1, 2]
        - 'hello'
    "};
    let value = dbt_yaml::from_str::<Value>(yaml).unwrap();
    let (list, unused_keys) = deserialize_value::<Vec<Untagged<Thing2>>>(value, |v| {
        if v.is_u64() {
            Ok(Some(Value::from(v.as_u64().unwrap() + 100)))
        } else if v.is_bool() {
            Ok(Some(Value::from(true)))
        } else {
            Ok(None)
        }
    });
    assert!(unused_keys.is_empty());
    assert_eq!(list.len(), 4);
    assert_eq!(
        list[0],
        Untagged::Thing(Thing2 {
            __untagged__: Untagged::Thing(Thing {
                a: 3.into(),
                b: None.into(),
                c: true,
            }),
            __rest__: HashMap::new(),
        })
    );
    assert_eq!(list[1], Untagged::Unit);
    assert_eq!(list[2], Untagged::Number(101, 102));
    assert_eq!(list[3], Untagged::String("hello".to_string()));
}

#[cfg(feature = "yaml_11")]
mod timestamp {
    use dbt_yaml::{TimeOfDay, Timestamp, Value};
    use std::cmp::Ordering;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    fn ts(
        year: i32,
        month: u8,
        day: u8,
        time: Option<TimeOfDay>,
        tz_minutes: Option<i32>,
    ) -> Value {
        Value::timestamp(Timestamp::new(year, month, day, time, tz_minutes))
    }

    #[test]
    fn value_eq_and_ord() {
        let date = ts(2001, 12, 15, None, None);
        let midnight = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), Some(0));
        assert_eq!(date, midnight);
        assert_eq!(date.partial_cmp(&midnight), Some(Ordering::Equal));
        assert!(date < ts(2001, 12, 16, None, None));

        // A timestamp is not equal to (nor ordered against) its string
        // representation.
        let string = Value::string("2001-12-15".into());
        assert_ne!(date, string);
        assert_eq!(date.partial_cmp(&string), None);
    }

    #[test]
    fn value_hash_is_consistent_with_eq() {
        fn hash(value: &Value) -> u64 {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        }
        let a = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        let b = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), None);
        assert_eq!(a, b);
        assert_eq!(hash(&a), hash(&b));
    }

    #[test]
    fn value_accessors() {
        let v = ts(2001, 12, 15, None, None);
        assert!(v.is_timestamp());
        assert_eq!(v.as_timestamp().unwrap().date(), (2001, 12, 15));
        assert!(!v.is_string());
        assert_eq!(v.as_str(), None);
    }

    #[test]
    fn value_debug() {
        let v = ts(2001, 12, 15, None, None);
        assert!(format!("{:?}", v).starts_with("Timestamp(2001-12-15) @"));
    }

    #[test]
    fn timestamp_as_mapping_key() {
        let mut mapping = dbt_yaml::Mapping::new();
        let key = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));
        mapping.insert(key, Value::bool(true));
        // A different spelling of the same instant is the same key.
        let same_instant = ts(2001, 12, 15, Some(TimeOfDay::new(0, 0, 0, 0)), None);
        assert_eq!(mapping.get(&same_instant), Some(&Value::bool(true)));
    }
    #[test]
    fn lenient_eq_matches_timestamp_against_string() {
        let midnight = ts(2001, 12, 15, None, None);
        let string = Value::string("2001-12-15".to_string());
        let offset = ts(2001, 12, 15, Some(TimeOfDay::new(2, 0, 0, 0)), Some(2 * 60));

        // Strict equality keeps Timestamp distinct from String...
        assert_ne!(midnight, string);

        // ...while lenient equality compares the normalized instant.
        assert!(midnight.lenient_eq(&string));
        assert!(string.lenient_eq(&midnight));
        assert!(string.lenient_eq(&offset));
        assert!(midnight.lenient_eq(&offset));

        assert!(!midnight.lenient_eq(&Value::string("2001-12-16".to_string())));
        assert!(!midnight.lenient_eq(&Value::string("not a timestamp".to_string())));
        assert!(!midnight.lenient_eq(&Value::null()));
    }

    #[test]
    fn lenient_eq_recurses_into_sequences_and_mappings() {
        use dbt_yaml::Mapping;

        let mut typed = Mapping::new();
        typed.insert(
            Value::string("created".to_string()),
            Value::sequence(vec![ts(2001, 12, 15, None, None)]),
        );
        let mut stringy = Mapping::new();
        stringy.insert(
            Value::string("created".to_string()),
            Value::sequence(vec![Value::string("2001-12-15".to_string())]),
        );

        let typed = Value::mapping(typed);
        let stringy = Value::mapping(stringy);

        assert_ne!(typed, stringy);
        assert!(typed.lenient_eq(&stringy));
        assert!(stringy.lenient_eq(&typed));
    }

    #[test]
    fn lenient_eq_compares_mapping_keys_strictly() {
        use dbt_yaml::Mapping;

        let mut typed = Mapping::new();
        typed.insert(ts(2001, 12, 15, None, None), Value::bool(true));
        let mut stringy = Mapping::new();
        stringy.insert(Value::string("2001-12-15".to_string()), Value::bool(true));

        let typed = Value::mapping(typed);
        let stringy = Value::mapping(stringy);

        assert!(!typed.lenient_eq(&stringy));
        assert!(!stringy.lenient_eq(&typed));
    }

    #[test]
    fn serde_boundary_delivers_string() {
        let v = ts(2001, 12, 15, Some(TimeOfDay::new(2, 59, 43, 0)), None);
        let s: String = dbt_yaml::from_value(v.clone()).unwrap();
        assert_eq!(s, "2001-12-15T02:59:43");

        let yaml = dbt_yaml::to_string(&v).unwrap();
        assert_eq!(yaml, "2001-12-15T02:59:43\n");
    }
}

/// Tests for the runtime helpers used by mixed (tagged + untagged variant)
/// enum dispatch.
mod mixed_enum_dispatch {
    use dbt_yaml::path::OwnedPath;
    use dbt_yaml::value::{
        extract_external_tag_and_deserializer_state, extract_optional_tag_and_deserializer_state,
        DeserializerState, ExternalTag,
    };
    use dbt_yaml::Value;
    use serde::de::IntoDeserializer;
    use serde::Deserialize as _;

    fn state_of(value: &Value) -> DeserializerState {
        DeserializerState::new(value.clone(), OwnedPath::Root, None, None)
    }

    fn state_value(state: &mut DeserializerState) -> Value {
        Value::deserialize(state.get_deserializer(None)).unwrap()
    }

    #[test]
    fn optional_tag_present_keeps_mapping_intact() {
        let value: Value = dbt_yaml::from_str("type: A\na: 1\n").unwrap();
        let (tag, mut state) =
            extract_optional_tag_and_deserializer_state(value.clone().into_deserializer(), "type")
                .unwrap();
        assert_eq!(tag.as_ref().and_then(Value::as_str), Some("A"));
        // The tag key is NOT removed: the state still holds the full mapping.
        assert_eq!(state_value(&mut state), value);
    }

    #[test]
    fn optional_tag_missing_or_not_a_mapping() {
        let value: Value = dbt_yaml::from_str("a: 1\n").unwrap();
        let (tag, _) =
            extract_optional_tag_and_deserializer_state(value.into_deserializer(), "type").unwrap();
        assert!(tag.is_none());

        let value: Value = dbt_yaml::from_str("- 1\n- 2\n").unwrap();
        let (tag, _) =
            extract_optional_tag_and_deserializer_state(value.into_deserializer(), "type").unwrap();
        assert!(tag.is_none());
    }

    #[test]
    fn strip_and_restore_tag_key() {
        let value: Value = dbt_yaml::from_str("type: A\na: 1\n").unwrap();
        let mut state = state_of(&value);

        let entry = state.strip_tag_key("type").unwrap();
        let stripped = state_value(&mut state);
        assert!(stripped.get("type").is_none());
        assert!(stripped.get("a").is_some());

        state.restore_tag_key(entry);
        let restored = state_value(&mut state);
        assert_eq!(restored.get("type").and_then(Value::as_str), Some("A"));
        assert!(restored.get("a").is_some());
        // The tag key is restored at its original position.
        match (&restored, &value) {
            (Value::Mapping(r, ..), Value::Mapping(v, ..)) => {
                assert!(r.keys().eq(v.keys()), "key order not preserved");
            }
            _ => panic!("expected mappings"),
        }

        // Stripping an absent key is a no-op.
        assert!(state.strip_tag_key("nope").is_none());
        assert_eq!(state_value(&mut state).get("a"), value.get("a"));
    }

    #[test]
    fn external_tag_shapes() {
        // Single-key mapping -> Wrapped(tag key)
        let value: Value = dbt_yaml::from_str("A:\n  x: 1\n").unwrap();
        let (tag, _) =
            extract_external_tag_and_deserializer_state(value.into_deserializer(), &[]).unwrap();
        match tag {
            Some(ExternalTag::Wrapped(key)) => assert_eq!(key.as_str(), Some("A")),
            other => panic!("expected Wrapped, got {other:?}"),
        }

        // Bare string scalar -> Bare
        let value: Value = dbt_yaml::from_str("Unit\n").unwrap();
        let (tag, _) =
            extract_external_tag_and_deserializer_state(value.into_deserializer(), &[]).unwrap();
        match tag {
            Some(ExternalTag::Bare(v)) => assert_eq!(v.as_str(), Some("Unit")),
            other => panic!("expected Bare, got {other:?}"),
        }

        // Multi-key mapping -> None
        let value: Value = dbt_yaml::from_str("x: 1\ny: 2\n").unwrap();
        let (tag, _) =
            extract_external_tag_and_deserializer_state(value.into_deserializer(), &[]).unwrap();
        assert!(tag.is_none());

        // Non-string scalar -> None
        let value: Value = dbt_yaml::from_str("42\n").unwrap();
        let (tag, _) =
            extract_external_tag_and_deserializer_state(value.into_deserializer(), &[]).unwrap();
        assert!(tag.is_none());
    }

    #[test]
    fn focus_and_restore_external_content() {
        let value: Value = dbt_yaml::from_str("A:\n  x: 1\n  y: 2\n").unwrap();
        let mut state = state_of(&value);

        let (original, index, key) = state.focus_external_content("A").unwrap();
        let inner = state_value(&mut state);
        assert_eq!(
            inner.get("x").and_then(Value::as_i64),
            Some(1),
            "focused value should be the inner content"
        );

        state.restore_external_content(original, index, key);
        let restored = state_value(&mut state);
        assert_eq!(restored, value);

        // Focusing a non-single-key mapping is a no-op.
        let value2: Value = dbt_yaml::from_str("x: 1\ny: 2\n").unwrap();
        let mut state2 = state_of(&value2);
        assert!(state2.focus_external_content("A").is_none());
        assert_eq!(state_value(&mut state2), value2);
    }

    #[test]
    fn focus_extends_path() {
        let value: Value = dbt_yaml::from_str("A:\n  x: 1\n  y: 2\n").unwrap();
        let mut state = state_of(&value);
        let _original = state.focus_external_content("A").unwrap();

        #[derive(serde_derive::Deserialize)]
        struct Inner {
            x: i32,
        }

        let mut unused = vec![];
        let mut cb = |path: dbt_yaml::Path<'_>, key: &Value, _value: &Value| {
            unused.push((path.to_string(), key.clone()));
        };
        let inner: Inner = Inner::deserialize(state.get_deserializer(Some(&mut cb))).unwrap();
        assert_eq!(inner.x, 1);
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].0, "A.y", "unused key path includes the tag key");
        assert_eq!(unused[0].1.as_str(), Some("y"));
    }
}

/// Parity tests for internally-tagged enums with untagged variants: the
/// `UntaggedEnumDeserialize` derive must behave like plain serde derive.
mod mixed_enum_internal_tag {
    use super::{deserialize_value, deserialize_value_inner};
    use dbt_yaml::Value;

    #[derive(Debug, PartialEq, serde_derive::Deserialize)]
    struct TaggedStruct {
        a: i32,
    }

    #[derive(Debug, PartialEq, serde_derive::Deserialize)]
    struct FallbackStruct {
        b: String,
    }

    mod via_serde {
        #[derive(Debug, PartialEq, serde_derive::Deserialize)]
        #[serde(tag = "type")]
        pub enum Mixed {
            Unit,
            Tagged(super::TaggedStruct),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
            #[serde(untagged)]
            Other(dbt_yaml::Value),
        }
    }

    mod via_derive {
        #[derive(Debug, PartialEq, dbt_yaml_derive::UntaggedEnumDeserialize)]
        #[serde(tag = "type")]
        pub enum Mixed {
            Unit,
            Tagged(super::TaggedStruct),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
            #[serde(untagged)]
            Other(dbt_yaml::Value),
        }
    }

    /// Structural equality, ignoring spans and mapping key order. The derive
    /// path preserves real spans while serde's Content buffering degrades
    /// them, so spans are excluded from parity comparison.
    fn semantic_eq(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null(..), Value::Null(..)) => true,
            (Value::Bool(x, ..), Value::Bool(y, ..)) => x == y,
            (Value::Number(x, ..), Value::Number(y, ..)) => x == y,
            (Value::String(x, ..), Value::String(y, ..)) => x == y,
            (Value::Sequence(x, ..), Value::Sequence(y, ..)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| semantic_eq(x, y))
            }
            (Value::Mapping(x, ..), Value::Mapping(y, ..)) => {
                x.len() == y.len()
                    && x.iter()
                        .all(|(k, v)| y.get(k).is_some_and(|v2| semantic_eq(v, v2)))
            }
            _ => false,
        }
    }

    /// Asserts that plain serde derive and `UntaggedEnumDeserialize` agree on
    /// the outcome for `yaml`, and returns the derived enum's result.
    fn check_parity(yaml: &str) -> Result<via_derive::Mixed, String> {
        let value: Value = dbt_yaml::from_str(yaml).unwrap();
        let expected = deserialize_value_inner::<via_serde::Mixed>(value.clone(), |_| Ok(None)).0;
        let actual = deserialize_value_inner::<via_derive::Mixed>(value, |_| Ok(None)).0;
        match (&expected, &actual) {
            (Ok(e), Ok(a)) => {
                let variant_matches = match (e, a) {
                    (via_serde::Mixed::Unit, via_derive::Mixed::Unit) => true,
                    (via_serde::Mixed::Tagged(x), via_derive::Mixed::Tagged(y)) => x == y,
                    (via_serde::Mixed::Fallback(x), via_derive::Mixed::Fallback(y)) => x == y,
                    (via_serde::Mixed::Other(x), via_derive::Mixed::Other(y)) => semantic_eq(x, y),
                    _ => false,
                };
                assert!(
                    variant_matches,
                    "parity mismatch for input {yaml:?}: {e:?} vs {a:?}"
                );
            }
            (Err(e), Err(a)) => {
                assert_eq!(
                    a.to_string().split(" at line").next().unwrap(),
                    e.to_string().split(" at line").next().unwrap(),
                    "error mismatch for input {yaml:?}"
                );
            }
            _ => panic!("outcome mismatch for input {yaml:?}: {expected:?} vs {actual:?}"),
        }
        actual.map_err(|e| e.to_string())
    }

    #[test]
    fn serde_parity_matrix() {
        let cases = [
            // Known tag, variant parses.
            "type: Unit\n",
            "type: Tagged\na: 1\n",
            // Known tag, variant fails: fall back to untagged variants.
            "type: Tagged\na: bad\n",
            "type: Tagged\na: bad\nb: hi\n",
            // Unknown tag: fall back to untagged variants.
            "type: Nope\nb: hi\n",
            "type: Nope\nx: 1\n",
            // Missing tag: untagged variants only.
            "b: hi\n",
            "c: true\n",
            // Non-mapping input: untagged variants only.
            "just a scalar\n",
        ];
        for yaml in cases {
            let _ = check_parity(yaml);
        }
    }

    /// Documented divergence: serde treats a sequence as `[tag, content]` and
    /// even accepts a numeric variant index as the tag (its bincode-oriented
    /// internally-tagged sequence form), so `[1, 2]` matches variant index 1.
    /// The derive does not replicate this: sequences go straight to the
    /// untagged variants.
    #[test]
    fn sequence_input_is_untagged_only() {
        let res = check_parity_ignoring_outcome("- 1\n- 2\n");
        assert!(matches!(
            res,
            Ok(via_derive::Mixed::Other(Value::Sequence(..)))
        ));
    }

    /// Runs only the derived enum, skipping the parity assertion.
    fn check_parity_ignoring_outcome(yaml: &str) -> Result<via_derive::Mixed, String> {
        let value: Value = dbt_yaml::from_str(yaml).unwrap();
        deserialize_value_inner::<via_derive::Mixed>(value, |_| Ok(None))
            .0
            .map_err(|e| e.to_string())
    }

    #[test]
    fn fallback_sees_tag_key() {
        // The untagged fallback runs against the full original value,
        // including the tag key.
        let res = check_parity("type: Tagged\na: bad\n").unwrap();
        match res {
            via_derive::Mixed::Other(v) => {
                assert_eq!(v.get("type").and_then(Value::as_str), Some("Tagged"));
                assert_eq!(v.get("a").and_then(Value::as_str), Some("bad"));
            }
            other => panic!("expected Other, got {other:?}"),
        }
    }

    #[test]
    fn known_tag_failure_falls_back_to_matching_untagged_variant() {
        let res = check_parity("type: Tagged\na: bad\nb: hi\n").unwrap();
        assert!(
            matches!(res, via_derive::Mixed::Fallback(FallbackStruct { ref b }) if b == "hi"),
            "expected Fallback, got {res:?}"
        );
    }

    #[test]
    fn unused_keys_forwarded_and_tag_key_filtered() {
        // Tagged match: extra keys are forwarded; the tag key is consumed by
        // the dispatch and never reported.
        let value: Value = dbt_yaml::from_str("type: Tagged\na: 1\nextra: 9\n").unwrap();
        let (res, unused) = deserialize_value::<via_derive::Mixed>(value, |_| Ok(None));
        assert!(matches!(res, via_derive::Mixed::Tagged(_)));
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].1.as_str(), Some("extra"));

        // Untagged match after an unknown tag: extra keys are forwarded; the
        // tag key is filtered out even though the variant saw it.
        let value: Value = dbt_yaml::from_str("type: Nope\nb: hi\nextra: 9\n").unwrap();
        let (res, unused) = deserialize_value::<via_derive::Mixed>(value, |_| Ok(None));
        assert!(matches!(res, via_derive::Mixed::Fallback(_)));
        assert_eq!(unused.len(), 1, "tag key must not be reported: {unused:?}");
        assert_eq!(unused[0].1.as_str(), Some("extra"));
    }

    mod no_catch_all {
        #[derive(Debug, PartialEq, serde_derive::Deserialize)]
        #[serde(tag = "type")]
        pub enum Mixed {
            Tagged(super::TaggedStruct),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
        }
    }

    mod via_derive_no_catch {
        #[derive(Debug, PartialEq, dbt_yaml_derive::UntaggedEnumDeserialize)]
        #[serde(tag = "type")]
        pub enum Mixed {
            Tagged(super::TaggedStruct),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
        }
    }

    /// Maps a deserialization result to a comparable outcome, stripping any
    /// location suffix from errors.
    fn outcome<T>(r: Result<T, dbt_yaml::Error>) -> Result<(), String> {
        r.map(|_| ())
            .map_err(|e| e.to_string().split(" at line").next().unwrap().to_string())
    }

    #[test]
    fn no_match_error_matches_serde() {
        let value: Value = dbt_yaml::from_str("c: true\n").unwrap();
        let expected =
            deserialize_value_inner::<no_catch_all::Mixed>(value.clone(), |_| Ok(None)).0;
        let actual = deserialize_value_inner::<via_derive_no_catch::Mixed>(value, |_| Ok(None)).0;

        let actual = outcome(actual);
        assert_eq!(actual, outcome(expected));
        assert_eq!(
            actual.unwrap_err(),
            "data did not match any variant of untagged enum Mixed"
        );
    }
}

/// Parity tests for externally-tagged enums with untagged variants: the
/// `UntaggedEnumDeserialize` derive must behave like plain serde derive.
mod mixed_enum_external {
    use super::{deserialize_value, deserialize_value_inner};
    use dbt_yaml::Value;

    #[derive(Debug, PartialEq, serde_derive::Deserialize)]
    struct TaggedStruct {
        a: i32,
    }

    #[derive(Debug, PartialEq, serde_derive::Deserialize)]
    struct FallbackStruct {
        b: String,
    }

    mod via_serde {
        #[derive(Debug, PartialEq, serde_derive::Deserialize)]
        pub enum Mixed {
            Unit,
            Tagged(super::TaggedStruct),
            NewType(i32),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
            #[serde(untagged)]
            Other(dbt_yaml::Value),
        }
    }

    mod via_derive {
        #[derive(Debug, PartialEq, dbt_yaml_derive::UntaggedEnumDeserialize)]
        pub enum Mixed {
            Unit,
            Tagged(super::TaggedStruct),
            NewType(i32),
            #[serde(untagged)]
            Fallback(super::FallbackStruct),
            #[serde(untagged)]
            Other(dbt_yaml::Value),
        }
    }

    /// Structural equality, ignoring spans and mapping key order. The derive
    /// path preserves real spans while serde's Content buffering degrades
    /// them, so spans are excluded from parity comparison.
    fn semantic_eq(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null(..), Value::Null(..)) => true,
            (Value::Bool(x, ..), Value::Bool(y, ..)) => x == y,
            (Value::Number(x, ..), Value::Number(y, ..)) => x == y,
            (Value::String(x, ..), Value::String(y, ..)) => x == y,
            (Value::Sequence(x, ..), Value::Sequence(y, ..)) => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| semantic_eq(x, y))
            }
            (Value::Mapping(x, ..), Value::Mapping(y, ..)) => {
                x.len() == y.len()
                    && x.iter()
                        .all(|(k, v)| y.get(k).is_some_and(|v2| semantic_eq(v, v2)))
            }
            _ => false,
        }
    }

    /// Asserts that plain serde derive and `UntaggedEnumDeserialize` agree on
    /// the outcome for `yaml`, and returns the derived enum's result.
    fn check_parity(yaml: &str) -> Result<via_derive::Mixed, String> {
        let value: Value = dbt_yaml::from_str(yaml).unwrap();
        let expected = deserialize_value_inner::<via_serde::Mixed>(value.clone(), |_| Ok(None)).0;
        let actual = deserialize_value_inner::<via_derive::Mixed>(value, |_| Ok(None)).0;
        match (&expected, &actual) {
            (Ok(e), Ok(a)) => {
                let variant_matches = match (e, a) {
                    (via_serde::Mixed::Unit, via_derive::Mixed::Unit) => true,
                    (via_serde::Mixed::Tagged(x), via_derive::Mixed::Tagged(y)) => x == y,
                    (via_serde::Mixed::NewType(x), via_derive::Mixed::NewType(y)) => x == y,
                    (via_serde::Mixed::Fallback(x), via_derive::Mixed::Fallback(y)) => x == y,
                    (via_serde::Mixed::Other(x), via_derive::Mixed::Other(y)) => semantic_eq(x, y),
                    _ => false,
                };
                assert!(
                    variant_matches,
                    "parity mismatch for input {yaml:?}: {e:?} vs {a:?}"
                );
            }
            (Err(e), Err(a)) => {
                assert_eq!(
                    a.to_string().split(" at line").next().unwrap(),
                    e.to_string().split(" at line").next().unwrap(),
                    "error mismatch for input {yaml:?}"
                );
            }
            _ => panic!("outcome mismatch for input {yaml:?}: {expected:?} vs {actual:?}"),
        }
        actual.map_err(|e| e.to_string())
    }

    #[test]
    fn serde_parity_matrix() {
        let cases = [
            // Unit variant forms.
            "Unit\n",
            "Unit: null\n",
            // Wrapped newtype variant.
            "NewType: 42\n",
            // Wrapped struct variant.
            "Tagged:\n  a: 1\n",
            // Known tag, variant fails: fall back to untagged variants on the
            // whole original value.
            "Unit: 5\n",
            "NewType: not-a-number\n",
            "Tagged:\n  a: bad\n",
            "Tagged:\n  a: bad\nb: hi\n",
            // Unknown tag: untagged variants see the whole wrapped value.
            "Nope: 1\n",
            // Single-key map whose key matches no variant: untagged variants.
            "b: hi\n",
            // Untagged variant names are not tags.
            "Fallback:\n  b: hi\n",
            // Not an externally-tagged shape: untagged variants only.
            "x: 1\ny: 2\n",
            "42\n",
            "- 1\n- 2\n",
        ];
        for yaml in cases {
            let _ = check_parity(yaml);
        }
    }

    #[test]
    fn fallback_sees_wrapper() {
        // The untagged fallback runs against the full original value,
        // including the tag wrapper.
        let res = check_parity("Tagged:\n  a: bad\n").unwrap();
        match res {
            via_derive::Mixed::Other(v) => {
                assert!(v.get("Tagged").is_some(), "wrapper key visible: {v:?}");
            }
            other => panic!("expected Other, got {other:?}"),
        }
    }

    #[test]
    fn unused_keys_forwarded_with_content_path() {
        // Tagged match: extra keys inside the content are forwarded with the
        // tag-extended path.
        let value: Value = dbt_yaml::from_str("Tagged:\n  a: 1\n  extra: 9\n").unwrap();
        let (res, unused) = deserialize_value::<via_derive::Mixed>(value, |_| Ok(None));
        assert!(matches!(res, via_derive::Mixed::Tagged(_)));
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].0, "Tagged.extra");
        assert_eq!(unused[0].1.as_str(), Some("extra"));
    }
}
