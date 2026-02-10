#![allow(dead_code)]
use indoc::indoc;
use std::collections::HashMap;

use dbt_yaml_schemars_derive::JsonSchema;

#[derive(JsonSchema)]
struct BasicStruct {
    #[schemars(skip)]
    a: String,
    #[schemars(skip_serializing)]
    b: f64,
    #[schemars(skip_deserializing)]
    c: i32,
    #[schemars(skip_serializing_if = "Option::is_none")]
    d: Option<String>,
    e: HashMap<String, String>,
}

#[derive(JsonSchema)]
enum BasicEnum {
    Unit,
    Tuple(String, i32),
    Struct { a: String, b: i32 },
    BasicStruct(BasicStruct),
}

#[derive(JsonSchema)]
struct DunderFieldStruct {
    a: String,
    __private__: HashMap<String, String>,
}

#[derive(JsonSchema)]
struct DunderFieldStructWithFlatten {
    a: String,
    __basic__: BasicStruct,
}

#[test]
fn test_struct_derive_jsonschema() {
    let schema = schemars::schema_for!(BasicStruct);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r#"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "BasicStruct",
  "type": "object",
  "required": [
    "b",
    "e"
  ],
  "properties": {
    "b": {
      "writeOnly": true,
      "type": "number",
      "format": "double"
    },
    "c": {
      "default": 0,
      "readOnly": true,
      "type": "integer",
      "format": "int32"
    },
    "d": {
      "type": [
        "string",
        "null"
      ]
    },
    "e": {
      "type": "object",
      "additionalProperties": {
        "type": "string"
      }
    }
  }
}"#}
    );
}

#[test]
fn test_enum_derive_jsonschema() {
    let schema = schemars::schema_for!(BasicEnum);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r##"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "BasicEnum",
  "oneOf": [
    {
      "type": "string",
      "enum": [
        "Unit"
      ]
    },
    {
      "type": "object",
      "required": [
        "Tuple"
      ],
      "properties": {
        "Tuple": {
          "type": "array",
          "items": [
            {
              "type": "string"
            },
            {
              "type": "integer",
              "format": "int32"
            }
          ],
          "maxItems": 2,
          "minItems": 2
        }
      },
      "additionalProperties": false
    },
    {
      "type": "object",
      "required": [
        "Struct"
      ],
      "properties": {
        "Struct": {
          "type": "object",
          "required": [
            "a",
            "b"
          ],
          "properties": {
            "a": {
              "type": "string"
            },
            "b": {
              "type": "integer",
              "format": "int32"
            }
          }
        }
      },
      "additionalProperties": false
    },
    {
      "type": "object",
      "required": [
        "BasicStruct"
      ],
      "properties": {
        "BasicStruct": {
          "$ref": "#/definitions/BasicStruct"
        }
      },
      "additionalProperties": false
    }
  ],
  "definitions": {
    "BasicStruct": {
      "type": "object",
      "required": [
        "b",
        "e"
      ],
      "properties": {
        "b": {
          "writeOnly": true,
          "type": "number",
          "format": "double"
        },
        "c": {
          "default": 0,
          "readOnly": true,
          "type": "integer",
          "format": "int32"
        },
        "d": {
          "type": [
            "string",
            "null"
          ]
        },
        "e": {
          "type": "object",
          "additionalProperties": {
            "type": "string"
          }
        }
      }
    }
  }
}"##}
    );
}

#[test]
fn test_dunder_field_struct_derive_jsonschema() {
    let schema = schemars::schema_for!(DunderFieldStruct);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    #[cfg(feature = "flatten_dunder")]
    assert_eq!(
        schema_json,
        indoc! {r#"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "DunderFieldStruct",
  "type": "object",
  "required": [
    "a"
  ],
  "properties": {
    "a": {
      "type": "string"
    }
  }
}"#}
    );
    #[cfg(not(feature = "flatten_dunder"))]
    assert_eq!(
        schema_json,
        indoc! {r#"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "DunderFieldStruct",
  "type": "object",
  "required": [
    "__private__",
    "a"
  ],
  "properties": {
    "__private__": {
      "type": "object",
      "additionalProperties": {
        "type": "string"
      }
    },
    "a": {
      "type": "string"
    }
  }
}"#}
    );
}

#[cfg(feature = "flatten_dunder")]
#[test]
fn test_dunder_field_struct_with_flatten_derive_jsonschema() {
    let schema = schemars::schema_for!(DunderFieldStructWithFlatten);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r#"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "DunderFieldStructWithFlatten",
  "type": "object",
  "required": [
    "a",
    "b",
    "e"
  ],
  "properties": {
    "a": {
      "type": "string"
    },
    "b": {
      "writeOnly": true,
      "type": "number",
      "format": "double"
    },
    "c": {
      "default": 0,
      "readOnly": true,
      "type": "integer",
      "format": "int32"
    },
    "d": {
      "type": [
        "string",
        "null"
      ]
    },
    "e": {
      "type": "object",
      "additionalProperties": {
        "type": "string"
      }
    }
  }
}"#}
    );
}
