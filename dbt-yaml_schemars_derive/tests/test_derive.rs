#![allow(dead_code)]
use dbt_yaml::{Value, Verbatim};
use indexmap::IndexMap;
use indoc::indoc;
use std::collections::HashMap;

use dbt_yaml_schemars_derive::{DbtSchema, JsonSchema};

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
    e: IndexMap<String, String>,
}

#[derive(JsonSchema)]
enum BasicEnum {
    Unit,
    Tuple(String, i32),
    Struct { a: String, b: i32 },
    BasicStruct(BasicStruct),
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

#[derive(JsonSchema)]
struct DunderFieldStruct {
    a: String,
    __private__: HashMap<String, f64>,
}

#[derive(JsonSchema)]
struct DunderFieldStructWithFlatten {
    a: String,
    __basic__: BasicStruct,
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
        "type": "number",
        "format": "double"
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

#[derive(JsonSchema)]
struct StructWithVerbatim {
    a: bool,
    b: Verbatim<Value, f64>,
}

#[test]
fn test_struct_with_verbatim_derive_jsonschema() {
    let schema = schemars::schema_for!(StructWithVerbatim);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r##"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "StructWithVerbatim",
  "type": "object",
  "required": [
    "a",
    "b"
  ],
  "properties": {
    "a": {
      "type": "boolean"
    },
    "b": {
      "type": "number",
      "format": "double"
    }
  }
}"##}
    );
}

#[derive(DbtSchema)]
struct DbtEntity {
    a: bool,
    b: Option<StringOrBool>,
    c: IntOrFloat,
    d: IndexMap<String, i64>,
    e: HashMap<String, Value>,
}

#[derive(DbtSchema)]
#[schemars(untagged)]
enum StringOrBool {
    String(String),
    Bool(bool),
}

#[derive(DbtSchema)]
#[schemars(untagged)]
enum IntOrFloat {
    Int(i64),
    Float(f64),
}

#[test]
fn test_dbt_schema_pre_transform() {
    dbt_yaml::maybe_transformable::set_generate_pre_transformation_schema(true);
    let schema = schemars::schema_for!(DbtEntity);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r###"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "DbtEntity",
  "type": "object",
  "required": [
    "a",
    "c",
    "d",
    "e"
  ],
  "properties": {
    "a": {
      "anyOf": [
        {
          "type": "boolean"
        },
        {
          "type": "string",
          "pattern": "^\\{.*\\}$"
        }
      ]
    },
    "b": {
      "anyOf": [
        {
          "$ref": "#/definitions/StringOrBool"
        },
        {
          "type": "null"
        }
      ]
    },
    "c": {
      "$ref": "#/definitions/IntOrFloat"
    },
    "d": {
      "type": "object",
      "additionalProperties": {
        "anyOf": [
          {
            "type": "integer",
            "format": "int64"
          },
          {
            "type": "string",
            "pattern": "^\\{.*\\}$"
          }
        ]
      }
    },
    "e": {
      "type": "object",
      "additionalProperties": {
        "$ref": "#/definitions/AnyValue"
      }
    }
  },
  "definitions": {
    "AnyValue": true,
    "IntOrFloat": {
      "anyOf": [
        {
          "type": "integer",
          "format": "int64"
        },
        {
          "type": "number",
          "format": "double"
        },
        {
          "type": "string",
          "pattern": "^\\{.*\\}$"
        }
      ]
    },
    "StringOrBool": {
      "anyOf": [
        {
          "type": "string"
        },
        {
          "type": "boolean"
        },
        {
          "type": "string",
          "pattern": "^\\{.*\\}$"
        }
      ]
    }
  }
}"###}
    );
}

#[test]
fn test_dbt_schema_post_transform() {
    dbt_yaml::maybe_transformable::set_generate_pre_transformation_schema(false);
    let schema = schemars::schema_for!(DbtEntity);
    let schema_json = serde_json::to_string_pretty(&schema).unwrap();
    println!("{}", schema_json);
    assert_eq!(
        schema_json,
        indoc! {r###"
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "DbtEntity",
  "type": "object",
  "required": [
    "a",
    "c",
    "d",
    "e"
  ],
  "properties": {
    "a": {
      "type": "boolean"
    },
    "b": {
      "anyOf": [
        {
          "$ref": "#/definitions/StringOrBool"
        },
        {
          "type": "null"
        }
      ]
    },
    "c": {
      "$ref": "#/definitions/IntOrFloat"
    },
    "d": {
      "type": "object",
      "additionalProperties": {
        "type": "integer",
        "format": "int64"
      }
    },
    "e": {
      "type": "object",
      "additionalProperties": {
        "$ref": "#/definitions/AnyValue"
      }
    }
  },
  "definitions": {
    "AnyValue": true,
    "IntOrFloat": {
      "anyOf": [
        {
          "type": "integer",
          "format": "int64"
        },
        {
          "type": "number",
          "format": "double"
        }
      ]
    },
    "StringOrBool": {
      "anyOf": [
        {
          "type": "string"
        },
        {
          "type": "boolean"
        }
      ]
    }
  }
}"###}
    );
}
