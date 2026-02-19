//! Integrationstests fuer den XML Serializer (EXI Events → XML).

use std::rc::Rc;

use erxi::event::{
    AtContent, ChContent, CmContent, DtContent, ErContent, ExiEvent, NsContent, PiContent,
};
use erxi::qname::QName;
use erxi::xml_serializer::events_to_xml;

// ============================================================================
// Hilfsfunktionen
// ============================================================================

fn se(local: &str) -> ExiEvent {
    ExiEvent::StartElement(Rc::new(QName::new("", local)))
}

fn se_ns(uri: &str, local: &str, prefix: &str) -> ExiEvent {
    ExiEvent::StartElement(Rc::new(QName::with_prefix(uri, local, prefix)))
}

fn at(local: &str, value: &str) -> ExiEvent {
    ExiEvent::Attribute(AtContent {
        qname: Rc::new(QName::new("", local)),
        value: value.into(),
    })
}

fn at_ns(uri: &str, local: &str, prefix: &str, value: &str) -> ExiEvent {
    ExiEvent::Attribute(AtContent {
        qname: Rc::new(QName::with_prefix(uri, local, prefix)),
        value: value.into(),
    })
}

fn ch(text: &str) -> ExiEvent {
    ExiEvent::Characters(ChContent {
        value: text.into(),
    })
}

fn ns(prefix: &str, uri: &str, local_element: bool) -> ExiEvent {
    ExiEvent::NamespaceDeclaration(NsContent {
        uri: uri.into(),
        prefix: prefix.into(),
        local_element_ns: local_element,
    })
}

fn cm(text: &str) -> ExiEvent {
    ExiEvent::Comment(CmContent { text: text.into() })
}

fn pi(name: &str, text: &str) -> ExiEvent {
    ExiEvent::ProcessingInstruction(PiContent {
        name: name.into(),
        text: text.into(),
    })
}

fn dt(name: &str, public: &str, system: &str, text: &str) -> ExiEvent {
    ExiEvent::DocType(DtContent {
        name: name.into(),
        public: public.into(),
        system: system.into(),
        text: text.into(),
    })
}

fn er(name: &str) -> ExiEvent {
    ExiEvent::EntityReference(ErContent { name: name.into() })
}

const SD: ExiEvent = ExiEvent::StartDocument;
const ED: ExiEvent = ExiEvent::EndDocument;
const EE: ExiEvent = ExiEvent::EndElement;

// ============================================================================
// Tests pro Event-Typ
// ============================================================================

/// Leeres Dokument mit einem Element.
#[test]
fn empty_document() {
    let events = vec![SD, se("root"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<root/>");
}

/// Verschachtelte Elemente.
#[test]
fn nested_elements() {
    let events = vec![SD, se("a"), se("b"), EE, EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<a><b/></a>");
}

/// Attribute.
#[test]
fn attributes() {
    let events = vec![SD, se("r"), at("a", "1"), at("b", "2"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<r a="1" b="2"/>"#);
}

/// Text-Inhalt.
#[test]
fn text_content() {
    let events = vec![SD, se("r"), ch("hello"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r>hello</r>");
}

/// Text-Escaping: & < > werden escaped.
#[test]
fn text_escaping() {
    let events = vec![SD, se("r"), ch("a&b<c"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r>a&amp;b&lt;c</r>");
}

/// Kommentar.
#[test]
fn comment() {
    let events = vec![SD, cm("test"), se("r"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<!--test--><r/>");
}

/// Processing Instruction.
#[test]
fn processing_instruction() {
    let events = vec![SD, pi("target", "data"), se("r"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<?target data?><r/>");
}

/// PI ohne Daten.
#[test]
fn processing_instruction_ohne_daten() {
    let events = vec![SD, pi("target", ""), se("r"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<?target?><r/>");
}

/// Namespace-Deklarationen.
#[test]
fn namespace_decls() {
    let events = vec![
        SD,
        se_ns("http://a", "x", "a"),
        ns("a", "http://a", true),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<a:x xmlns:a="http://a"/>"#);
}

/// DOCTYPE mit SYSTEM.
#[test]
fn doctype_system() {
    let events = vec![SD, dt("root", "", "root.dtd", ""), se("root"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<!DOCTYPE root SYSTEM "root.dtd"><root/>"#);
}

/// DOCTYPE mit PUBLIC.
#[test]
fn doctype_public() {
    let events = vec![
        SD,
        dt("html", "-//W3C//DTD XHTML 1.0//EN", "xhtml.dtd", ""),
        se("html"),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(
        xml,
        r#"<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0//EN" "xhtml.dtd"><html/>"#
    );
}

/// DOCTYPE mit internem Subset.
#[test]
fn doctype_internal_subset() {
    let events = vec![
        SD,
        dt("doc", "", "", r#"<!ENTITY copy "(c)"> "#),
        se("doc"),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(
        xml,
        r#"<!DOCTYPE doc [<!ENTITY copy "(c)"> ]><doc/>"#
    );
}

/// Entity Reference.
#[test]
fn entity_reference() {
    let events = vec![SD, se("r"), er("foo"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r>&foo;</r>");
}

/// Mixed Content.
#[test]
fn mixed_content() {
    let events = vec![SD, se("r"), ch("a"), se("b"), EE, ch("c"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r>a<b/>c</r>");
}

/// Attribut-Escaping: " wird zu &quot;.
#[test]
fn attr_escaping() {
    let events = vec![SD, se("r"), at("x", "a\"b"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<r x="a&quot;b"/>"#);
}

/// Default-Namespace (leerer Prefix).
#[test]
fn default_ns() {
    let events = vec![
        SD,
        se("r"),
        ns("", "http://x", true),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<r xmlns="http://x"/>"#);
}

/// xsi:type Clark-Notation wird zu prefix:local aufgeloest.
#[test]
fn xsi_type_clark_notation() {
    let events = vec![
        SD,
        se_ns("http://example.org", "root", "ex"),
        ns("ex", "http://example.org", true),
        ns("xsi", "http://www.w3.org/2001/XMLSchema-instance", false),
        at_ns(
            "http://www.w3.org/2001/XMLSchema-instance",
            "type",
            "xsi",
            "{http://example.org}MyType",
        ),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert!(xml.contains(r#"xsi:type="ex:MyType""#), "xsi:type Clark → prefixed; got: {xml}");
}

/// SelfContained wird ignoriert.
#[test]
fn self_contained_ignoriert() {
    let events = vec![SD, se("r"), ExiEvent::SelfContained, EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r/>");
}

/// Element mit Attributen und Text.
#[test]
fn element_mit_attr_und_text() {
    let events = vec![SD, se("p"), at("class", "note"), ch("Hallo"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, r#"<p class="note">Hallo</p>"#);
}

/// Tief verschachtelte Elemente.
#[test]
fn deep_nesting() {
    let events = vec![SD, se("a"), se("b"), se("c"), ch("x"), EE, EE, EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<a><b><c>x</c></b></a>");
}

/// Namespace-Attribut mit URI-Lookup (Attribut hat URI aber keinen Prefix,
/// wird aus NS-Bindings aufgeloest).
#[test]
fn attr_ns_lookup() {
    let events = vec![
        SD,
        se("r"),
        ns("ns1", "http://ns1", false),
        at_ns("http://ns1", "val", "", "42"),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    // Attribut sollte ns1:val sein, da URI aus NS-Binding aufgeloest wird
    assert!(xml.contains(r#"ns1:val="42""#), "Attr NS lookup; got: {xml}");
}

/// Mehrere NS-Deklarationen auf einem Element.
#[test]
fn mehrere_ns_decls() {
    let events = vec![
        SD,
        se_ns("http://a", "root", "a"),
        ns("a", "http://a", true),
        ns("b", "http://b", false),
        EE,
        ED,
    ];
    let xml = events_to_xml(&events).unwrap();
    assert!(xml.contains(r#"xmlns:a="http://a""#), "NS a; got: {xml}");
    assert!(xml.contains(r#"xmlns:b="http://b""#), "NS b; got: {xml}");
}

/// > in Textinhalt wird escaped.
#[test]
fn text_gt_escaping() {
    let events = vec![SD, se("r"), ch("a>b"), EE, ED];
    let xml = events_to_xml(&events).unwrap();
    assert_eq!(xml, "<r>a&gt;b</r>");
}

// ============================================================================
// Round-Trip-Test: XML → Events → XML → Events (Events vergleichen)
// ============================================================================

/// Round-Trip: parse_xml_events → events_to_xml → parse_xml_events, Events vergleichen.
#[test]
fn round_trip_simple() {
    use erxi::xml::parse_xml_events;
    use std::io::Write;

    // Einfaches XML in Temp-Datei schreiben
    let xml_input = "<root><child attr=\"val\">text</child></root>";
    let dir = std::env::temp_dir().join("erxi_test_roundtrip");
    let _ = std::fs::create_dir_all(&dir);
    let path = dir.join("simple.xml");
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(xml_input.as_bytes()).unwrap();

    let events1 = parse_xml_events(&path, false).unwrap();
    let xml_out = events_to_xml(&events1).unwrap();

    // Zweiten Durchgang: xml_out parsen
    let path2 = dir.join("simple_rt.xml");
    std::fs::write(&path2, &xml_out).unwrap();
    let events2 = parse_xml_events(&path2, false).unwrap();

    assert_eq!(events1, events2, "Round-trip Events muessen gleich sein");
}

// ============================================================================
// Full-Pipeline-Test: XML → EXI → XML
// ============================================================================

/// Full-Pipeline: XML → parse → encode EXI → decode EXI → events_to_xml → parse → vergleiche.
#[test]
fn full_round_trip_xml_exi_xml() {
    use erxi::decoder::decode;
    use erxi::encoder::encode;
    use erxi::options::ExiOptions;
    use erxi::xml::parse_xml_events;
    use std::io::Write;

    let xml_input = "<root><item id=\"1\">Hallo</item><item id=\"2\">Welt</item></root>";
    let dir = std::env::temp_dir().join("erxi_test_full_rt");
    let _ = std::fs::create_dir_all(&dir);
    let path = dir.join("full_rt.xml");
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(xml_input.as_bytes()).unwrap();

    // 1. XML → Events
    let events_original = parse_xml_events(&path, false).unwrap();

    // 2. Events → EXI
    let opts = ExiOptions::default();
    let exi_bytes = encode(&events_original, &opts).unwrap();

    // 3. EXI → Events
    let (events_decoded, _) = decode(&exi_bytes).unwrap();

    // 4. Events → XML
    let xml_out = events_to_xml(&events_decoded).unwrap();

    // 5. XML → Events (nochmal parsen zum Vergleich)
    let path2 = dir.join("full_rt_out.xml");
    std::fs::write(&path2, &xml_out).unwrap();
    let events_final = parse_xml_events(&path2, false).unwrap();

    assert_eq!(
        events_decoded, events_final,
        "Full round-trip: decoded Events muessen nach XML-Roundtrip gleich sein"
    );
}
