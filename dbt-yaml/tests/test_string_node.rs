use dbt_yaml::{Span, StringNode};
use indoc::indoc;

fn scalar(s: &str) -> StringNode {
    StringNode::Scalar(s.to_owned(), Span::zero())
}

fn sequence(items: Vec<StringNode>) -> StringNode {
    StringNode::Sequence(items, Span::zero())
}

fn mapping(pairs: Vec<(&str, StringNode)>) -> StringNode {
    StringNode::Mapping(
        pairs.into_iter().map(|(k, v)| (scalar(k), v)).collect(),
        Span::zero(),
    )
}

#[test]
fn test_plain_scalars_stay_strings() {
    for (yaml, expected) in [
        ("123", "123"),
        ("0x10", "0x10"),
        ("0o17", "0o17"),
        ("01", "01"),
        ("1e3", "1e3"),
        ("1.0", "1.0"),
        ("1_000", "1_000"),
        ("true", "true"),
        ("yes", "yes"),
        ("Off", "Off"),
        ("~", "~"),
        ("null", "null"),
        (".inf", ".inf"),
        (".nan", ".nan"),
        ("2001-02-03", "2001-02-03"),
    ] {
        assert_eq!(StringNode::from_str(yaml).unwrap(), scalar(expected));
    }
}

#[test]
fn test_empty_document() {
    assert_eq!(StringNode::from_str("").unwrap(), scalar(""));
    assert_eq!(StringNode::from_str("---\n").unwrap(), scalar(""));
}

#[test]
fn test_quoted_scalars() {
    assert_eq!(StringNode::from_str("'123'").unwrap(), scalar("123"));
    // The parser resolves escape sequences in double-quoted scalars.
    assert_eq!(StringNode::from_str("\"a\\nb\"").unwrap(), scalar("a\nb"));
    assert_eq!(
        StringNode::from_str("|\n  literal\n").unwrap(),
        scalar("literal\n")
    );
}

#[test]
fn test_tagged_scalar_stays_string() {
    assert_eq!(StringNode::from_str("!!int 42").unwrap(), scalar("42"));
    assert_eq!(StringNode::from_str("!custom foo").unwrap(), scalar("foo"));
}

#[test]
fn test_nested_structure() {
    let yaml = indoc! {"
        name: dbt-yaml
        version: 0.10.5
        features:
          - filename
          - yaml_11
        matrix:
          fast: true
          retries: 3
    "};
    let expected = mapping(vec![
        ("name", scalar("dbt-yaml")),
        ("version", scalar("0.10.5")),
        (
            "features",
            sequence(vec![scalar("filename"), scalar("yaml_11")]),
        ),
        (
            "matrix",
            mapping(vec![("fast", scalar("true")), ("retries", scalar("3"))]),
        ),
    ]);
    assert_eq!(StringNode::from_str(yaml).unwrap(), expected);
}

#[test]
fn test_numeric_mapping_key_stays_string() {
    let yaml = "107: yes\n";
    assert_eq!(
        StringNode::from_str(yaml).unwrap(),
        mapping(vec![("107", scalar("yes"))])
    );
}

#[test]
fn test_non_scalar_mapping_key() {
    let yaml = "? [a, b]\n: pair key\n";
    let expected = StringNode::Mapping(
        vec![(sequence(vec![scalar("a"), scalar("b")]), scalar("pair key"))],
        Span::zero(),
    );
    assert_eq!(StringNode::from_str(yaml).unwrap(), expected);
}

#[test]
fn test_flow_style() {
    let yaml = "{a: [1, {b: 2}], c: []}";
    let expected = mapping(vec![
        (
            "a",
            sequence(vec![scalar("1"), mapping(vec![("b", scalar("2"))])]),
        ),
        ("c", sequence(vec![])),
    ]);
    assert_eq!(StringNode::from_str(yaml).unwrap(), expected);
}

#[test]
fn test_alias() {
    let yaml = indoc! {"
        defaults: &defaults
          port: 5432
        service:
          <<: *defaults
          host: localhost
    "};
    let defaults = mapping(vec![("port", scalar("5432"))]);
    let expected = mapping(vec![
        ("defaults", defaults.clone()),
        (
            "service",
            StringNode::Mapping(
                vec![
                    (scalar("<<"), defaults),
                    (scalar("host"), scalar("localhost")),
                ],
                Span::zero(),
            ),
        ),
    ]);
    assert_eq!(StringNode::from_str(yaml).unwrap(), expected);
}

#[test]
fn test_alias_cycle_is_error() {
    let yaml = "&a [*a]\n";
    assert!(StringNode::from_str(yaml).is_err());
}

#[test]
fn test_multiple_documents_error() {
    let yaml = "---\na: 1\n---\nb: 2\n";
    assert!(StringNode::from_str(yaml).is_err());
}

#[test]
fn test_parse_error() {
    let yaml = "a: [1, 2\n";
    assert!(StringNode::from_str(yaml).is_err());
}

#[test]
fn test_unknown_anchor_error() {
    assert!(StringNode::from_str("*bogus\n").is_err());
}

#[test]
fn test_from_slice_and_reader() {
    let yaml = "k: 0x10\n";
    let expected = mapping(vec![("k", scalar("0x10"))]);
    assert_eq!(StringNode::from_slice(yaml.as_bytes()).unwrap(), expected);
    assert_eq!(StringNode::from_reader(yaml.as_bytes()).unwrap(), expected);
}

#[test]
fn test_spans() {
    let yaml = indoc! {"
        name: dbt-yaml
        features:
          - filename
          - yaml_11
    "};
    let node = StringNode::from_str(yaml).unwrap();

    // The top-level mapping spans the whole document.
    assert_eq!(
        (node.span().start.line, node.span().start.column),
        (1, 1)
    );
    assert_eq!((node.span().end.line, node.span().end.column), (5, 1));

    let StringNode::Mapping(pairs, _) = &node else {
        panic!("expected mapping");
    };

    // The "name" key runs to the start of its value.
    let (key, value) = &pairs[0];
    assert_eq!((key.span().start.line, key.span().start.column), (1, 1));
    assert_eq!((key.span().end.line, key.span().end.column), (1, 7));
    assert_eq!(
        (value.span().start.line, value.span().start.column),
        (1, 7)
    );
    assert_eq!((value.span().end.line, value.span().end.column), (2, 1));

    // The "features" sequence starts at the first `-` entry and runs to the
    // end of the document.
    let (key, value) = &pairs[1];
    assert_eq!((key.span().start.line, key.span().start.column), (2, 1));
    let StringNode::Sequence(items, seq_span) = value else {
        panic!("expected sequence");
    };
    assert_eq!((seq_span.start.line, seq_span.start.column), (3, 3));
    assert_eq!((seq_span.end.line, seq_span.end.column), (5, 1));
    assert_eq!(
        (items[0].span().start.line, items[0].span().start.column),
        (3, 5)
    );
    assert_eq!(
        (items[0].span().end.line, items[0].span().end.column),
        (4, 5)
    );
}

#[test]
fn test_span_of_empty_document() {
    let node = StringNode::from_str("").unwrap();
    assert_eq!(
        (node.span().start.line, node.span().start.column),
        (1, 1)
    );
}

#[test]
fn test_alias_span_is_alias_reference() {
    let yaml = indoc! {"
        defaults: &defaults
          port: 5432
        service: *defaults
    "};
    let node = StringNode::from_str(yaml).unwrap();
    let StringNode::Mapping(pairs, _) = &node else {
        panic!("expected mapping");
    };
    let (_, aliased) = &pairs[1];
    let StringNode::Mapping(aliased_pairs, _) = aliased else {
        panic!("expected mapping");
    };
    assert_eq!(aliased_pairs[0].0, scalar("port"));
    assert_eq!(
        (aliased.span().start.line, aliased.span().start.column),
        (3, 10)
    );
}
