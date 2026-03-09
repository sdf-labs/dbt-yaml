#![allow(dead_code)]

use dbt_yaml_schemars_derive::DbtSchema;

#[derive(DbtSchema)]
struct A {
    b: B,
}

#[derive(DbtSchema)]
struct B {
    c: C,
}

#[derive(DbtSchema)]
struct C {
    val: i32,
}

#[derive(DbtSchema)]
struct DeepFlattenA {
    #[schemars(flatten)]
    b: DeepFlattenB,
}

#[derive(DbtSchema)]
struct DeepFlattenB {
    #[schemars(flatten)]
    c: DeepFlattenC,
}

#[derive(DbtSchema)]
struct DeepFlattenC {
    val: i32,
}

#[test]
fn test_dbt_schema() {
    let schema = schemars::schema_for!(A);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);

    let schema2 = schemars::schema_for!(DeepFlattenA);
    let schema_json2 = serde_json::to_string_pretty(&schema2).unwrap();
    println!("{}", schema_json2);
}
