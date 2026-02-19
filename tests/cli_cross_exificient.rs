use erxi::options::{Alignment, ExiOptions};
use std::cell::RefCell;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

fn options_to_exificient_args(opts: &ExiOptions, schema_path: &str) -> Vec<String> {
    let mut args = vec!["-schema".to_string(), schema_path.to_string()];

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

fn erxi_bin() -> &'static str {
    env!("CARGO_BIN_EXE_erxi")
}

fn run_erxi(args: &[&str]) -> Output {
    Command::new(erxi_bin())
        .args(args)
        .output()
        .expect("run erxi")
}

fn test_temp_dir(tag: &str) -> PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("erxi-cli-cross-{tag}-{}-{ts}", std::process::id()));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn ensure_exificient_env() -> bool {
    if Command::new("java").arg("-version").output().is_err() {
        eprintln!("SKIP: Java nicht verfügbar");
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

    if std::env::var("EXIFICIENT_JAR").is_err() {
        let cp = jars
            .iter()
            .map(|p| p.to_string_lossy().to_string())
            .collect::<Vec<_>>()
            .join(":");
        unsafe {
            std::env::set_var("EXIFICIENT_JAR", cp);
        }
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
                eprintln!("SKIP: javac fehlgeschlagen für tools/ExifBatch.java");
                return false;
            }
        }
    }

    true
}

fn assert_infoset_eq(expected_xml: &str, actual_xml: &str) {
    let opts = ExiOptions::default();
    let expected = erxi::parse_xml_events_from_str(expected_xml, &opts).expect("parse expected");
    let actual = erxi::parse_xml_events_from_str(actual_xml, &opts).expect("parse actual");
    assert_eq!(expected, actual);
}

#[test]
fn cross_erxi_cli_no_include_options_to_exificient_decode() {
    if !ensure_exificient_env() {
        return;
    }

    let dir = test_temp_dir("no-include-to-exif");
    let xml = "<root><item>abc</item><item>def</item></root>";
    let xsd = r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"#;
    let xml_path = dir.join("in.xml");
    let xsd_path = dir.join("schema.xsd");
    let exi_path = dir.join("out.exi");
    let exif_decoded = dir.join("exif-decoded.xml");
    fs::write(&xml_path, xml).expect("write xml");
    fs::write(&xsd_path, xsd).expect("write xsd");

    let enc = run_erxi(&[
        "encode",
        "-i",
        xml_path.to_str().unwrap(),
        "-o",
        exi_path.to_str().unwrap(),
        "--schema",
        xsd_path.to_str().unwrap(),
        "--byte-aligned",
        "--no-include-options",
    ]);
    assert!(enc.status.success(), "erxi encode failed: {}", String::from_utf8_lossy(&enc.stderr));

    let opts = ExiOptions::default().with_alignment(Alignment::ByteAlignment);
    exificient_decode(
        &exi_path,
        &opts,
        xsd_path.to_str().unwrap(),
        &exif_decoded,
    )
    .expect("exificient decode failed");

    let xml_out = fs::read_to_string(exif_decoded).expect("read decoded");
    assert_infoset_eq(xml, &xml_out);
}

#[test]
fn cross_exificient_encode_to_erxi_cli_decode() {
    if !ensure_exificient_env() {
        return;
    }

    let dir = test_temp_dir("exif-to-erxi");
    let xml = "<root><v>hello</v><v>world</v></root>";
    let xsd = r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="v" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"#;
    let xml_path = dir.join("in.xml");
    let xsd_path = dir.join("schema.xsd");
    let exi_path = dir.join("exif.exi");
    let erxi_decoded = dir.join("erxi-decoded.xml");
    fs::write(&xml_path, xml).expect("write xml");
    fs::write(&xsd_path, xsd).expect("write xsd");

    let opts = ExiOptions::default()
        .with_compression()
        .with_preserve(erxi::Preserve {
            prefixes: true,
            ..erxi::Preserve::default()
        });

    exificient_encode(
        &xml_path,
        &opts,
        xsd_path.to_str().unwrap(),
        &exi_path,
    )
    .expect("exificient encode failed");

    let dec = run_erxi(&[
        "decode",
        "-i",
        exi_path.to_str().unwrap(),
        "-o",
        erxi_decoded.to_str().unwrap(),
        "--schema",
        xsd_path.to_str().unwrap(),
        "--compression",
        "--preserve-prefixes",
    ]);
    assert!(dec.status.success(), "erxi decode failed: {}", String::from_utf8_lossy(&dec.stderr));

    let xml_out = fs::read_to_string(erxi_decoded).expect("read decoded");
    assert_infoset_eq(xml, &xml_out);
}
