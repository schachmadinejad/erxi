use erxi::encoder::{encode_with_config, encode_with_schema_and_config, EncoderConfig};
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions, Preserve, SchemaId};
use erxi::xml_serializer::events_to_xml;
use erxi::parse_xml_events_from_str;
use std::cell::RefCell;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

fn assert_infoset_eq(expected_xml: &str, actual_xml: &str, opts: &ExiOptions) {
    let expected = parse_xml_events_from_str(expected_xml, opts).expect("parse expected");
    let actual = parse_xml_events_from_str(actual_xml, opts).expect("parse actual");
    assert_eq!(expected, actual, "infoset mismatch\nexpected: {expected_xml}\nactual: {actual_xml}");
}

fn test_temp_dir(tag: &str) -> PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("erxi-coverage-{tag}-{}-{ts}", std::process::id()));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn options_to_exificient_args(opts: &ExiOptions, schema_path: &str) -> Vec<String> {
    let mut args = Vec::new();
    if !schema_path.is_empty() {
        args.push("-schema".to_string());
        args.push(schema_path.to_string());
    }

    match opts.alignment() {
        Alignment::ByteAlignment => args.push("-bytePacked".to_string()),
        Alignment::PreCompression => args.push("-preCompression".to_string()),
        _ => {}
    }
    if opts.compression() {
        args.push("-compression".to_string());
    }
    if opts.strict() {
        args.push("-strict".to_string());
    }
    if opts.preserve().comments {
        args.push("-preserveComments".to_string());
    }
    if opts.preserve().pis {
        args.push("-preservePIs".to_string());
    }
    if opts.preserve().dtd {
        args.push("-preserveDTDs".to_string());
    }
    if opts.preserve().prefixes {
        args.push("-preservePrefixes".to_string());
    }
    if opts.fragment() {
        args.push("-fragment".to_string());
    }
    args
}

include!("common/exif_batch.rs");

fn ensure_exificient_env() -> bool {
    if Command::new("java").arg("-version").output().is_err() {
        eprintln!("SKIP: Java nicht verfuegbar");
        return false;
    }

    let jar_dir = Path::new("tests/fixtures/exificient");
    let jars = [
        jar_dir.join("exificient-1.0.4.jar"),
        jar_dir.join("exificient-core-1.0.4.jar"),
        jar_dir.join("exificient-grammars-1.0.4.jar"),
        jar_dir.join("xercesImpl-2.12.0.jar"),
    ];
    if jars.iter().any(|p| !p.exists()) {
        eprintln!("SKIP: Exificient JARs fehlen in tests/fixtures/exificient");
        return false;
    }

    let cp = jars
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join(":");
    unsafe {
        std::env::set_var("EXIFICIENT_JAR", cp);
    }

    let exifbatch_java = Path::new("tools/ExifBatch.java");
    let exifbatch_class = Path::new("tools/ExifBatch.class");
    if !exifbatch_class.exists() {
        let cp = std::env::var("EXIFICIENT_JAR").expect("EXIFICIENT_JAR missing");
        let status = Command::new("javac")
            .args(["-cp", &cp, exifbatch_java.to_str().expect("java path")])
            .status();
        match status {
            Ok(s) if s.success() => {}
            _ => {
                eprintln!("SKIP: javac fehlgeschlagen fuer tools/ExifBatch.java");
                return false;
            }
        }
    }

    true
}

#[test]
fn interop_schema_id_variants_exificient_decode() {
    if !ensure_exificient_env() {
        return;
    }

    let xml = "<root><v>1</v></root>";
    let events = parse_xml_events_from_str(xml, &ExiOptions::default()).expect("parse xml");

    let dir = test_temp_dir("schema-id-interop");
    let exi_path = dir.join("out.exi");
    let decoded_path = dir.join("out.xml");

    // schemaId=None
    let mut opts_none = ExiOptions::default();
    opts_none.set_schema_id(Some(SchemaId::None));
    let exi = encode_with_config(&events, &opts_none, EncoderConfig { include_options: true, ..EncoderConfig::default() })
        .expect("encode schemaId=None");
    fs::write(&exi_path, exi).expect("write exi");
    exificient_decode(&exi_path, &opts_none, "", &decoded_path).expect("exificient decode none");
    let xml_out = fs::read_to_string(&decoded_path).expect("read decoded");
    assert_infoset_eq(xml, &xml_out, &ExiOptions::default());

    // schemaId=BuiltinOnly
    let mut opts_builtin = ExiOptions::default();
    opts_builtin.set_schema_id(Some(SchemaId::BuiltinOnly));
    let exi = encode_with_config(&events, &opts_builtin, EncoderConfig { include_options: true, ..EncoderConfig::default() })
        .expect("encode schemaId=builtin");
    fs::write(&exi_path, exi).expect("write exi");
    exificient_decode(&exi_path, &opts_builtin, "", &decoded_path).expect("exificient decode builtin");
    let xml_out = fs::read_to_string(&decoded_path).expect("read decoded");
    assert_infoset_eq(xml, &xml_out, &ExiOptions::default());

    // schemaId=Id with schema-informed encoding
    let xsd = r#"
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          <xs:element name="root">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="v" type="xs:int"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:schema>
    "#;
    let schema = erxi::xsd::parse_xsd(xsd).expect("parse xsd");
    let schema_path = dir.join("schema.xsd");
    fs::write(&schema_path, xsd).expect("write schema");

    let mut opts_id = ExiOptions::default();
    opts_id.set_schema_id(Some(SchemaId::Id(schema_path.to_string_lossy().to_string())));
    opts_id.set_strict(true);

    let exi = encode_with_schema_and_config(
        &events,
        &opts_id,
        &schema,
        EncoderConfig { include_options: true, ..EncoderConfig::default() },
    )
    .expect("encode schemaId=id");
    fs::write(&exi_path, exi).expect("write exi");
    exificient_decode(
        &exi_path,
        &opts_id,
        schema_path.to_str().expect("schema path"),
        &decoded_path,
    )
    .expect("exificient decode id");
    let xml_out = fs::read_to_string(&decoded_path).expect("read decoded");
    assert_infoset_eq(xml, &xml_out, &ExiOptions::default());
}

#[test]
fn alignment_and_compression_roundtrip_matrix() {
    let xml = "<root><v>alpha</v><v>beta</v><v>gamma</v><v>delta</v></root>";
    let base_opts = ExiOptions::default();

    let variants: Vec<(&str, Box<dyn Fn(&mut ExiOptions)>)> = vec![
        ("bitpacked", Box::new(|_| {})),
        ("bytealigned", Box::new(|o| o.set_alignment(Alignment::ByteAlignment))),
        ("precompression", Box::new(|o| o.set_alignment(Alignment::PreCompression))),
        ("compression", Box::new(|o| { o.set_compression(true); o.set_block_size(64); })),
    ];

    for (name, apply) in variants {
        let mut opts = base_opts.clone();
        apply(&mut opts);
        let events = parse_xml_events_from_str(xml, &opts).unwrap_or_else(|e| panic!("{name}: parse xml: {e}"));
        let exi = erxi::encoder::encode(&events, &opts).unwrap_or_else(|e| panic!("{name}: encode: {e}"));
        let (decoded, _) = erxi::decode(&exi).unwrap_or_else(|e| panic!("{name}: decode: {e}"));
        let xml_out = events_to_xml(&decoded).unwrap_or_else(|e| panic!("{name}: to_xml: {e}"));
        assert_infoset_eq(xml, &xml_out, &ExiOptions::default());
    }
}

#[test]
fn value_limits_and_eviction_roundtrip() {
    let xml = "<root><v>alpha</v><v>bravo</v><v>charlie</v><v>delta</v><v>echo</v></root>";
    let mut opts = ExiOptions::default();
    opts.set_value_max_length(Some(4));
    opts.set_value_partition_capacity(Some(4));

    let events = parse_xml_events_from_str(xml, &opts).expect("parse xml");
    let exi = erxi::encoder::encode(&events, &opts).expect("encode");
    let (decoded, _) = erxi::decode(&exi).expect("decode");
    let xml_out = events_to_xml(&decoded).expect("to_xml");
    assert_infoset_eq(xml, &xml_out, &ExiOptions::default());
}

#[test]
fn preserve_prefixes_roundtrip() {
    let xml = r#"<ns1:root xmlns:ns1="urn:a" xmlns:ns2="urn:b"><ns2:child ns1:attr="v"/></ns1:root>"#;
    let mut opts = ExiOptions::default();
    opts.set_preserve(Preserve { prefixes: true, ..Preserve::default() });

    let events = parse_xml_events_from_str(xml, &opts).expect("parse xml");
    let exi = erxi::encoder::encode(&events, &opts).expect("encode");
    let (decoded, _) = erxi::decode(&exi).expect("decode");
    let xml_out = events_to_xml(&decoded).expect("to_xml");
    assert_infoset_eq(xml, &xml_out, &opts);
}

#[test]
fn dtrm_strict_schema_id_roundtrip() {
    let xsd = r#"
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          <xs:element name="root">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="price" type="xs:decimal"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:schema>
    "#;
    let schema = erxi::xsd::parse_xsd(xsd).expect("parse xsd");

    let dir = test_temp_dir("dtrm-schemaid");
    let schema_path = dir.join("schema.xsd");
    fs::write(&schema_path, xsd).expect("write schema");

    let xml = "<root><price>12.50</price></root>";
    let mut opts = ExiOptions::default();
    opts.set_strict(true);
    opts.set_schema_id(Some(SchemaId::Id(schema_path.to_string_lossy().to_string())));
    opts.set_datatype_representation_map(vec![DatatypeRepresentationMapping {
        type_qname: erxi::QName::new("http://www.w3.org/2001/XMLSchema", "decimal"),
        representation_qname: erxi::QName::new("http://www.w3.org/2009/exi", "string"),
    }]);

    let events = parse_xml_events_from_str(xml, &opts).expect("parse xml");
    let exi = encode_with_schema_and_config(
        &events,
        &opts,
        &schema,
        EncoderConfig { include_options: true, ..EncoderConfig::default() },
    )
    .expect("encode");

    let (decoded, decoded_opts) = erxi::decode_with_schema(&exi, ExiOptions::default(), &schema)
        .expect("decode");
    let xml_out = events_to_xml(&decoded).expect("to_xml");
    assert_infoset_eq(xml, &xml_out, &ExiOptions::default());

    assert_eq!(decoded_opts.schema_id(), opts.schema_id());
    assert_eq!(decoded_opts.datatype_representation_map().len(), 1);
    assert_eq!(decoded_opts.datatype_representation_map()[0], opts.datatype_representation_map()[0]);
}
