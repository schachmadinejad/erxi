use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn erxi_bin() -> &'static str {
    env!("CARGO_BIN_EXE_erxi")
}

fn test_temp_dir(tag: &str) -> PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("erxi-cli-e2e-{tag}-{}-{ts}", std::process::id()));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn run_erxi(args: &[&str]) -> Output {
    Command::new(erxi_bin())
        .args(args)
        .output()
        .expect("run erxi")
}

fn write_xml(path: &PathBuf, xml: &str) {
    fs::write(path, xml).expect("write xml");
}

fn assert_infoset_eq(expected_xml: &str, actual_xml: &str) {
    let opts = erxi::ExiOptions::default();
    let expected = erxi::parse_xml_events_from_str(expected_xml, &opts).expect("parse expected");
    let actual = erxi::parse_xml_events_from_str(actual_xml, &opts).expect("parse actual");
    assert_eq!(expected, actual);
}

#[test]
fn cli_include_cookie_writes_exi_magic() {
    let dir = test_temp_dir("cookie");
    let xml = "<root><a>1</a></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let decoded = dir.join("out.xml");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--include-cookie",
        "--include-options",
    ]);
    assert!(enc.status.success(), "encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let bytes = fs::read(&exi).expect("read exi");
    assert!(bytes.starts_with(b"$EXI"), "missing EXI cookie");

    let dec = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded.to_str().unwrap(),
    ]);
    assert!(dec.status.success(), "decode failed: {}", String::from_utf8_lossy(&dec.stderr));
    let out = fs::read_to_string(decoded).expect("read decoded");
    assert_infoset_eq(xml, &out);
}

#[test]
fn cli_auto_include_options_for_byte_aligned() {
    let dir = test_temp_dir("auto-opts");
    let xml = "<root><x>abc</x></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let decoded = dir.join("out.xml");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--byte-aligned",
    ]);
    assert!(enc.status.success(), "encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let dec = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded.to_str().unwrap(),
    ]);
    assert!(dec.status.success(), "decode failed: {}", String::from_utf8_lossy(&dec.stderr));
    let out = fs::read_to_string(decoded).expect("read decoded");
    assert_infoset_eq(xml, &out);
}

#[test]
fn cli_no_include_options_requires_matching_out_of_band_decode_options() {
    let dir = test_temp_dir("no-opts");
    let xml = "<root><x>abc</x></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let decoded_default = dir.join("out-default.xml");
    let decoded_aligned = dir.join("out-aligned.xml");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--byte-aligned",
        "--no-include-options",
    ]);
    assert!(enc.status.success(), "encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let dec_default = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded_default.to_str().unwrap(),
    ]);
    assert!(
        !dec_default.status.success(),
        "decode without out-of-band options unexpectedly succeeded"
    );

    let dec_aligned = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded_aligned.to_str().unwrap(),
        "--byte-aligned",
    ]);
    assert!(
        dec_aligned.status.success(),
        "decode with explicit byte-aligned failed: {}",
        String::from_utf8_lossy(&dec_aligned.stderr)
    );
    let out = fs::read_to_string(decoded_aligned).expect("read decoded");
    assert_infoset_eq(xml, &out);
}

#[test]
fn cli_parallel_deflate_requires_compression() {
    let dir = test_temp_dir("parallel-requires");
    let xml = "<root><x>abc</x></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--parallel-deflate",
    ]);
    assert!(!enc.status.success(), "expected clap constraint failure");
}

#[test]
fn cli_parallel_deflate_and_no_memory_monitor_roundtrip() {
    let dir = test_temp_dir("parallel-ok");
    let xml = "<root><x>abc</x><y>def</y></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let decoded = dir.join("out.xml");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--compression",
        "--parallel-deflate",
        "--no-memory-monitor",
        "--include-options",
    ]);
    assert!(enc.status.success(), "encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let dec = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded.to_str().unwrap(),
        "--no-memory-monitor",
    ]);
    assert!(dec.status.success(), "decode failed: {}", String::from_utf8_lossy(&dec.stderr));
    let out = fs::read_to_string(decoded).expect("read decoded");
    assert_infoset_eq(xml, &out);
}

#[test]
fn cli_schema_id_none_and_builtin_reject_with_schema() {
    let dir = test_temp_dir("schema-id-guard");
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let xsd = dir.join("schema.xsd");
    write_xml(&input, "<root>v</root>");
    fs::write(
        &xsd,
        r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root" type="xs:string"/>
</xs:schema>"#,
    )
    .expect("write xsd");

    let enc_none = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--schema",
        xsd.to_str().unwrap(),
        "--schema-id-none",
    ]);
    assert!(!enc_none.status.success(), "expected schema-id-none guard");

    let enc_builtin = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--schema",
        xsd.to_str().unwrap(),
        "--schema-id-builtin",
    ]);
    assert!(!enc_builtin.status.success(), "expected schema-id-builtin guard");
}

#[test]
fn cli_roundtrip_with_value_limits_and_lexical_whitespace_flags() {
    let dir = test_temp_dir("limits-lexical");
    let xml = "<root><v> 00123 </v><v> 00456 </v></root>";
    let input = dir.join("in.xml");
    let exi = dir.join("out.exi");
    let decoded = dir.join("out.xml");
    write_xml(&input, xml);

    let enc = run_erxi(&[
        "encode",
        "-i",
        input.to_str().unwrap(),
        "-o",
        exi.to_str().unwrap(),
        "--compression",
        "--include-options",
        "--preserve-lexical",
        "--preserve-whitespace",
        "--block-size",
        "64",
        "--value-max-length",
        "8",
        "--value-capacity",
        "16",
    ]);
    assert!(enc.status.success(), "encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let dec = run_erxi(&[
        "decode",
        "-i",
        exi.to_str().unwrap(),
        "-o",
        decoded.to_str().unwrap(),
        "--preserve-whitespace",
        "--value-max-length",
        "8",
        "--value-capacity",
        "16",
    ]);
    assert!(dec.status.success(), "decode failed: {}", String::from_utf8_lossy(&dec.stderr));
    let out = fs::read_to_string(decoded).expect("read decoded");
    assert_infoset_eq(xml, &out);
}
