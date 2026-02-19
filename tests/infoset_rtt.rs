//! Infoset Mapping Round-Trip-Tests (Spec Appendix B).
//!
//! Fuer jeden der 11 XML Information Set Item-Typen (B.1–B.11) ein
//! expliziter Round-Trip-Test: XML → Encode → Decode → Events vergleichen.
//! Jeder Test laeuft ueber alle 4 Alignments (bitpacked, bytealigned,
//! precompression, compression).

use erxi::decoder::decode_with_options;
use erxi::encoder::{encode_with_config, EncoderConfig};
use erxi::event::{
    AtContent, ChContent, CmContent, DtContent, ErContent, ExiEvent, NsContent, PiContent,
};
use erxi::options::{Alignment, ExiOptions, Preserve};
use erxi::xml::parse_xml_events_with_options;
use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};

// ============================================================================
// Alignment-Konfigurationen
// ============================================================================

const ALIGNMENTS: &[(&str, fn(&mut ExiOptions))] = &[
    ("bitpacked", |_| {}),
    ("bytealigned", |o| o.set_alignment(Alignment::ByteAlignment)),
    (
        "precompression",
        |o| o.set_alignment(Alignment::PreCompression),
    ),
    ("compression", |o| o.set_compression(true)),
];

// ============================================================================
// Hilfsfunktionen
// ============================================================================

/// Atomarer Zaehler fuer eindeutige temporaere Dateinamen.
static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Schreibt XML-String in temporaere Datei, parst Events mit den gegebenen
/// Options, encodiert zu EXI, decodiert zurueck und gibt die Events zurueck.
fn round_trip(xml: &str, opts: &ExiOptions) -> Vec<ExiEvent> {
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!("erxi_infoset_rtt_{n}.xml"));
    {
        let mut f = std::fs::File::create(&path).expect("tempfile erstellen");
        f.write_all(xml.as_bytes())
            .expect("XML in tempfile schreiben");
    }

    let events = parse_xml_events_with_options(&path, opts)
        .unwrap_or_else(|e| panic!("XML-Parse-Fehler: {e}\nXML: {xml}"));

    let _ = std::fs::remove_file(&path);

    let config = EncoderConfig { include_cookie: false, include_options: true };
    let exi = encode_with_config(&events, opts, config)
        .unwrap_or_else(|e| panic!("Encode-Fehler: {e}\nEvents: {events:?}"));

    let (decoded, _opts) = decode_with_options(&exi, opts.clone())
        .unwrap_or_else(|e| panic!("Decode-Fehler: {e}\nEXI: {} bytes", exi.len()));

    decoded
}

/// Fuehrt einen Round-Trip-Test fuer alle 4 Alignments durch.
/// `check_fn` prueft die decodierten Events.
fn test_all_alignments(
    xml: &str,
    base_opts: ExiOptions,
    check_fn: fn(&[ExiEvent], &str),
) {
    for &(name, apply) in ALIGNMENTS {
        let mut opts = base_opts.clone();
        apply(&mut opts);
        let events = round_trip(xml, &opts);
        check_fn(&events, name);
    }
}

// ============================================================================
// B.1 Document Information Item
// ============================================================================

/// Spec Appendix B.1: Ein Dokument-Info-Item wird durch SD/ED Events abgebildet.
/// Die [children]-Eigenschaft enthaelt den Document Element als SE/EE Paar.
#[test]
fn b1_document_round_trip() {
    let xml = "<root/>";
    let opts = ExiOptions::default();

    test_all_alignments(xml, opts, |events, align| {
        // SD, SE(root), EE, ED
        assert!(
            events.len() >= 4,
            "{align}: Erwartet mind. 4 Events, got {}",
            events.len()
        );
        assert!(
            matches!(events.first(), Some(ExiEvent::StartDocument)),
            "{align}: Erstes Event muss SD sein"
        );
        assert!(
            matches!(events.last(), Some(ExiEvent::EndDocument)),
            "{align}: Letztes Event muss ED sein"
        );
        // SE(root) finden
        let has_se_root = events.iter().any(|e| {
            matches!(e, ExiEvent::StartElement(q) if &*q.local_name == "root" && q.uri.is_empty())
        });
        assert!(has_se_root, "{align}: SE(root) erwartet");
    });
}

// ============================================================================
// B.2 Element Information Items
// ============================================================================

/// Spec Appendix B.2: Element-Info-Items werden durch SE/EE-Paare abgebildet.
/// [namespace name], [local name], [prefix] sind im SE Event.
/// [children] enthalten verschachtelte SE/EE, CH, PI, CM, ER Events.
#[test]
fn b2_element_round_trip() {
    let xml = "<root><a><b/></a><c/></root>";
    let opts = ExiOptions::default();

    test_all_alignments(xml, opts, |events, align| {
        // SD, SE(root), SE(a), SE(b), EE, EE, SE(c), EE, EE, ED
        assert_eq!(events.len(), 10, "{align}: Erwartet 10 Events");
        assert_eq!(events[0], ExiEvent::StartDocument);
        assert!(matches!(&events[1], ExiEvent::StartElement(q) if &*q.local_name == "root"));
        assert!(matches!(&events[2], ExiEvent::StartElement(q) if &*q.local_name == "a"));
        assert!(matches!(&events[3], ExiEvent::StartElement(q) if &*q.local_name == "b"));
        assert_eq!(events[4], ExiEvent::EndElement); // EE(b)
        assert_eq!(events[5], ExiEvent::EndElement); // EE(a)
        assert!(matches!(&events[6], ExiEvent::StartElement(q) if &*q.local_name == "c"));
        assert_eq!(events[7], ExiEvent::EndElement); // EE(c)
        assert_eq!(events[8], ExiEvent::EndElement); // EE(root)
        assert_eq!(events[9], ExiEvent::EndDocument);
    });
}

// ============================================================================
// B.3 Attribute Information Item
// ============================================================================

/// Spec Appendix B.3: Attribut-Info-Items werden durch AT Events abgebildet.
/// [namespace name], [local name], [prefix] und [normalized value] im AT Event.
#[test]
fn b3_attribute_round_trip() {
    let xml = r#"<root a="1" b="2"><child x="y"/></root>"#;
    let opts = ExiOptions::default();

    test_all_alignments(xml, opts, |events, align| {
        // Attribute sammeln
        let attrs: Vec<&AtContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::Attribute(at) => Some(at),
                _ => None,
            })
            .collect();

        assert_eq!(attrs.len(), 3, "{align}: Erwartet 3 Attribute");

        // a="1" und b="2" auf root
        let a = attrs.iter().find(|at| &*at.qname.local_name == "a");
        let b = attrs.iter().find(|at| &*at.qname.local_name == "b");
        let x = attrs.iter().find(|at| &*at.qname.local_name == "x");

        assert!(a.is_some(), "{align}: AT(a) erwartet");
        assert_eq!(&*a.unwrap().value, "1", "{align}: AT(a) Wert");
        assert!(b.is_some(), "{align}: AT(b) erwartet");
        assert_eq!(&*b.unwrap().value, "2", "{align}: AT(b) Wert");
        assert!(x.is_some(), "{align}: AT(x) erwartet");
        assert_eq!(&*x.unwrap().value, "y", "{align}: AT(x) Wert");
    });
}

// ============================================================================
// B.4 Processing Instruction Information Item
// ============================================================================

/// Spec Appendix B.4: PI-Info-Items werden durch PI Events abgebildet.
/// [target] und [content] sind im PI Event. Erfordert preserve.pis=true.
#[test]
fn b4_pi_round_trip() {
    let xml = "<?target data?><root/>";
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            pis: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let pis: Vec<&PiContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::ProcessingInstruction(pi) => Some(pi),
                _ => None,
            })
            .collect();

        assert_eq!(pis.len(), 1, "{align}: Erwartet 1 PI Event");
        assert_eq!(&*pis[0].name, "target", "{align}: PI target");
        assert_eq!(&*pis[0].text, "data", "{align}: PI text");
    });
}

// ============================================================================
// B.5 Unexpanded Entity Reference Information Item
// ============================================================================

/// Spec Appendix B.5: Unexpanded Entity Reference Info-Items werden durch
/// ER Events abgebildet. [name] ist im ER Event. Erfordert preserve.dtd=true.
///
/// "Unexpanded" bedeutet: Entities die NICHT expandiert werden koennen (typisch:
/// externe Entities mit SYSTEM-Referenz). Bei preserve.dtd=true erzeugt der
/// XML-Parser ER-Events fuer externe Entity-Referenzen.
#[test]
fn b5_entity_reference_round_trip() {
    // Externe Entity: kann vom Parser nicht aufgeloest werden → ER Event
    let xml = r#"<!DOCTYPE root [<!ENTITY ext SYSTEM "external.xml">]><root>&ext;</root>"#;
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let ers: Vec<&ErContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::EntityReference(er) => Some(er),
                _ => None,
            })
            .collect();

        assert_eq!(ers.len(), 1, "{align}: Erwartet 1 ER Event");
        assert_eq!(&*ers[0].name, "ext", "{align}: ER name");
    });
}

// ============================================================================
// B.6 Character Information Item
// ============================================================================

/// Spec Appendix B.6: Zeichen-Info-Items werden durch CH Events abgebildet.
/// [character code] entspricht den einzelnen Zeichen im CH Event.
#[test]
fn b6_character_round_trip() {
    let xml = "<root>Hello &amp; World</root>";
    let opts = ExiOptions::default();

    test_all_alignments(xml, opts, |events, align| {
        let chs: Vec<&ChContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::Characters(ch) => Some(ch),
                _ => None,
            })
            .collect();

        // quick-xml loest &amp; auf → "Hello & World" in einem CH Event
        assert!(!chs.is_empty(), "{align}: Erwartet mind. 1 CH Event");
        let combined: String = chs.iter().map(|ch| &*ch.value).collect();
        assert_eq!(combined, "Hello & World", "{align}: CH Wert");
    });
}

// ============================================================================
// B.7 Comment Information Item
// ============================================================================

/// Spec Appendix B.7: Kommentar-Info-Items werden durch CM Events abgebildet.
/// [content] ist im CM Event. Erfordert preserve.comments=true.
#[test]
fn b7_comment_round_trip() {
    let xml = "<!-- comment --><root><!-- inner --></root>";
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            comments: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let cms: Vec<&CmContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::Comment(cm) => Some(cm),
                _ => None,
            })
            .collect();

        assert_eq!(cms.len(), 2, "{align}: Erwartet 2 CM Events");
        assert_eq!(&*cms[0].text, " comment ", "{align}: CM[0] text");
        assert_eq!(&*cms[1].text, " inner ", "{align}: CM[1] text");
    });
}

// ============================================================================
// B.8 Document Type Declaration Information Item
// ============================================================================

/// Spec Appendix B.8: DOCTYPE-Info-Items werden durch DT Events abgebildet.
/// [system identifier] und [public identifier] sind im DT Event.
/// Erfordert preserve.dtd=true.
#[test]
fn b8_doctype_round_trip() {
    let xml = r#"<!DOCTYPE root SYSTEM "root.dtd"><root/>"#;
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let dts: Vec<&DtContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::DocType(dt) => Some(dt),
                _ => None,
            })
            .collect();

        assert_eq!(dts.len(), 1, "{align}: Erwartet 1 DT Event");
        assert_eq!(&*dts[0].name, "root", "{align}: DT name");
        assert_eq!(&*dts[0].system, "root.dtd", "{align}: DT system");
        assert!(dts[0].public.is_empty(), "{align}: DT public leer");
    });
}

// ============================================================================
// B.9 Unparsed Entity Information Item
// ============================================================================

/// Spec Appendix B.9: Unparsed-Entity-Info-Items werden ueber den text-Content
/// des DT Events abgebildet. Die Entity/Notation-Infos sind im internen
/// DTD-Subset enthalten. Erfordert preserve.dtd=true.
///
/// Hinweis: Externe Entity-Deklarationen (SYSTEM/PUBLIC) werden im
/// reconstruct_internal_subset uebersprungen (wie Exificient). Der Test
/// prueft, dass die NOTATION-Deklaration im DT text erhalten bleibt.
#[test]
fn b9_unparsed_entity_round_trip() {
    let xml = r#"<!DOCTYPE root [<!NOTATION jpeg SYSTEM "jpeg"><!ENTITY pic SYSTEM "pic.jpg" NDATA jpeg>]><root/>"#;
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let dts: Vec<&DtContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::DocType(dt) => Some(dt),
                _ => None,
            })
            .collect();

        assert_eq!(dts.len(), 1, "{align}: Erwartet 1 DT Event");
        assert_eq!(&*dts[0].name, "root", "{align}: DT name");
        // NOTATION-Deklaration muss im text erhalten sein (B.10)
        assert!(
            dts[0].text.contains("NOTATION"),
            "{align}: DT text muss NOTATION enthalten, got: {:?}",
            dts[0].text
        );
        assert!(
            dts[0].text.contains("jpeg"),
            "{align}: DT text muss 'jpeg' enthalten"
        );
    });
}

// ============================================================================
// B.10 Notation Information Item
// ============================================================================

/// Spec Appendix B.10: Notation-Info-Items werden ueber den text-Content des
/// DT Events abgebildet. [name], [system identifier] sind im DT text.
/// Wird zusammen mit B.9 getestet (Notation ist im DTD-Subset enthalten).
#[test]
fn b10_notation_round_trip() {
    // Gleiche Fixture wie B.9 — NOTATION jpeg SYSTEM "jpeg" muss erhalten bleiben
    let xml = r#"<!DOCTYPE root [<!NOTATION jpeg SYSTEM "image/jpeg">]><root/>"#;
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let dts: Vec<&DtContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::DocType(dt) => Some(dt),
                _ => None,
            })
            .collect();

        assert_eq!(dts.len(), 1, "{align}: Erwartet 1 DT Event");
        assert!(
            dts[0].text.contains("NOTATION"),
            "{align}: DT text muss NOTATION enthalten, got: {:?}",
            dts[0].text
        );
        assert!(
            dts[0].text.contains("jpeg"),
            "{align}: DT text muss 'jpeg' enthalten"
        );
        assert!(
            dts[0].text.contains("image/jpeg"),
            "{align}: DT text muss system identifier enthalten"
        );
    });
}

// ============================================================================
// B.11 Namespace Information Item
// ============================================================================

/// Spec Appendix B.11: Namespace-Info-Items werden durch NS Events abgebildet.
/// [prefix] und [namespace name] sind im NS Event.
/// Erfordert preserve.prefixes=true.
#[test]
fn b11_namespace_round_trip() {
    let xml = r#"<root xmlns:a="http://a" xmlns:b="http://b"><a:x/><b:y/></root>"#;
    let opts = ExiOptions::default()
        .with_preserve(Preserve {
            prefixes: true,
            ..Default::default()
        });

    test_all_alignments(xml, opts, |events, align| {
        let nss: Vec<&NsContent> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::NamespaceDeclaration(ns) => Some(ns),
                _ => None,
            })
            .collect();

        // Mindestens 2 NS-Deklarationen auf root (a→http://a, b→http://b)
        // Plus ggf. NS auf a:x und b:y (local_element_ns)
        assert!(
            nss.len() >= 2,
            "{align}: Erwartet mind. 2 NS Events, got {}",
            nss.len()
        );

        let ns_a = nss.iter().find(|ns| &*ns.prefix == "a");
        let ns_b = nss.iter().find(|ns| &*ns.prefix == "b");

        assert!(ns_a.is_some(), "{align}: NS(a) erwartet");
        assert_eq!(&*ns_a.unwrap().uri, "http://a", "{align}: NS(a) uri");
        assert!(ns_b.is_some(), "{align}: NS(b) erwartet");
        assert_eq!(&*ns_b.unwrap().uri, "http://b", "{align}: NS(b) uri");

        // SE(a:x) und SE(b:y) muessen korrekte URIs haben
        let ses: Vec<_> = events
            .iter()
            .filter_map(|e| match e {
                ExiEvent::StartElement(q) => Some(q),
                _ => None,
            })
            .collect();

        let se_x = ses.iter().find(|q| &*q.local_name == "x");
        let se_y = ses.iter().find(|q| &*q.local_name == "y");
        assert!(se_x.is_some(), "{align}: SE(x) erwartet");
        assert_eq!(&*se_x.unwrap().uri, "http://a", "{align}: SE(x) uri");
        assert!(se_y.is_some(), "{align}: SE(y) erwartet");
        assert_eq!(&*se_y.unwrap().uri, "http://b", "{align}: SE(y) uri");
    });
}
