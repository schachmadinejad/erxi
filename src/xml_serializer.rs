//! EXI Events → XML Serialisierung (Spec Appendix B, Infoset Mapping).
//!
//! Konvertiert eine Sequenz von EXI Events zurueck in XML.
//! Unterstuetzt alle 12 Event-Typen inkl. Namespace-Handling und xsi:type
//! Clark-Notation Rueckkonvertierung.
//!
//! Vier APIs:
//! - `events_to_xml()` — gibt XML als String zurueck (Convenience).
//! - `events_to_xml_writer()` — streamt XML direkt in `impl Write` (kein String im RAM).
//! - `events_to_xml_iter()` — streamt aus einem infallible Iterator in `impl Write`.
//! - `events_to_xml_iter_fallible()` — streamt aus einem fallible Iterator in `impl Write`.

use std::io::Write;
use std::rc::Rc;

use crate::error::Error;
use crate::event::{ExiEvent, NsContent};
use crate::qname::QName;
use crate::Result;

/// Serialisiert EXI-Events als XML-String.
pub fn events_to_xml(events: &[ExiEvent]) -> Result<String> {
    let mut buf = Vec::new();
    events_to_xml_writer(events, &mut buf)?;
    String::from_utf8(buf).map_err(|_| Error::IoError("XML output is not valid UTF-8".into()))
}

/// Serialisiert EXI-Events direkt in einen Writer (Streaming, kein String im RAM).
pub fn events_to_xml_writer(events: &[ExiEvent], writer: impl Write) -> Result<()> {
    let mut ser = XmlStreamSerializer::new(writer);
    for event in events {
        ser.process(event)?;
    }
    ser.finish()
}

/// Serialisiert EXI-Events aus einem Iterator direkt in einen Writer.
pub fn events_to_xml_iter(
    events: impl Iterator<Item = ExiEvent>,
    writer: impl Write,
) -> Result<()> {
    let mut ser = XmlStreamSerializer::new(writer);
    for event in events {
        ser.process(&event)?;
    }
    ser.finish()
}

/// Wie `events_to_xml_iter`, aber für fallible Iteratoren (Decode-Fehler via `?`).
pub fn events_to_xml_iter_fallible(
    events: impl Iterator<Item = Result<ExiEvent>>,
    writer: impl Write,
) -> Result<()> {
    let mut ser = XmlStreamSerializer::new(writer);
    for event_result in events {
        ser.process(&event_result?)?;
    }
    ser.finish()
}

/// Streaming XML-Serializer — schreibt direkt in `W: Write`.
pub(crate) struct XmlStreamSerializer<W: Write> {
    writer: W,
    element_stack: Vec<(Rc<QName>, Vec<NsContent>)>,
    pending_start: Option<Rc<QName>>,
    pending_ns: Vec<NsContent>,
    pending_attrs: Vec<(Rc<QName>, Rc<str>)>,
    synthetic_ns_counter: usize,
}

impl<W: Write> XmlStreamSerializer<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            element_stack: Vec::new(),
            pending_start: None,
            pending_ns: Vec::new(),
            pending_attrs: Vec::new(),
            synthetic_ns_counter: 0,
        }
    }

    pub fn process(&mut self, event: &ExiEvent) -> Result<()> {
        match event {
            ExiEvent::StartDocument => {}
            ExiEvent::EndDocument => {
                self.flush_pending_start(false)?;
            }
            ExiEvent::StartElement(qname) => {
                self.flush_pending_start(false)?;
                self.pending_start = Some(qname.clone());
                self.pending_ns.clear();
                self.pending_attrs.clear();
            }
            ExiEvent::NamespaceDeclaration(ns) => {
                self.pending_ns.push(ns.clone());
            }
            ExiEvent::Attribute(at) => {
                self.pending_attrs
                    .push((at.qname.clone(), at.value.clone()));
            }
            ExiEvent::EndElement => {
                if self.pending_start.is_some() {
                    self.flush_pending_start(true)?;
                } else {
                    let (qname, _ns) = self.element_stack.pop().ok_or_else(|| {
                        Error::XmlParseError("EndElement ohne offenes Element".into())
                    })?;
                    self.w("</")?;
                    self.write_qname(&qname)?;
                    self.w(">")?;
                }
            }
            ExiEvent::Characters(ch) => {
                self.flush_pending_start(false)?;
                write_escaped_text(&mut self.writer, &ch.value)?;
            }
            ExiEvent::Comment(cm) => {
                self.flush_pending_start(false)?;
                if cm.text.contains("--") || cm.text.ends_with('-') {
                    return Err(Error::XmlParseError(
                        "Kommentar-Text enthaelt '--' oder endet mit '-' (XML 1.0 Section 2.5)"
                            .into(),
                    ));
                }
                self.w("<!--")?;
                self.w(&cm.text)?;
                self.w("-->")?;
            }
            ExiEvent::ProcessingInstruction(pi) => {
                self.flush_pending_start(false)?;
                if pi.text.contains("?>") {
                    return Err(Error::XmlParseError(
                        "PI-Data enthaelt '?>' (XML 1.0 Section 2.6)".into(),
                    ));
                }
                self.w("<?")?;
                self.w(&pi.name)?;
                if !pi.text.is_empty() {
                    self.w(" ")?;
                    self.w(&pi.text)?;
                }
                self.w("?>")?;
            }
            ExiEvent::DocType(dt) => {
                self.flush_pending_start(false)?;
                self.w("<!DOCTYPE ")?;
                self.w(&dt.name)?;
                if !dt.public.is_empty() {
                    self.w(" PUBLIC \"")?;
                    write_escaped_attr(&mut self.writer, &dt.public)?;
                    self.w("\" \"")?;
                    write_escaped_attr(&mut self.writer, &dt.system)?;
                    self.w("\"")?;
                } else if !dt.system.is_empty() {
                    self.w(" SYSTEM \"")?;
                    write_escaped_attr(&mut self.writer, &dt.system)?;
                    self.w("\"")?;
                }
                if !dt.text.is_empty() {
                    self.w(" [")?;
                    self.w(&dt.text)?;
                    self.w("]")?;
                }
                self.w(">")?;
            }
            ExiEvent::EntityReference(er) => {
                self.flush_pending_start(false)?;
                self.w("&")?;
                self.w(&er.name)?;
                self.w(";")?;
            }
            ExiEvent::SelfContained => {}
        }
        Ok(())
    }

    pub fn finish(mut self) -> Result<()> {
        self.writer.flush().map_err(io_err)?;
        Ok(())
    }

    /// Schreibt den gepufferten Start-Tag und pusht das Element auf den Stack.
    fn flush_pending_start(&mut self, self_closing: bool) -> Result<()> {
        let Some(qname) = self.pending_start.take() else {
            return Ok(());
        };

        let mut ns_decls = std::mem::take(&mut self.pending_ns);
        let attrs = std::mem::take(&mut self.pending_attrs);

        self.synthesize_missing_ns(&qname, &attrs, &mut ns_decls);

        self.w("<")?;
        self.write_qname(&qname)?;

        // NS-Deklarationen schreiben
        for ns in &ns_decls {
            if ns.prefix.is_empty() {
                self.w(" xmlns=\"")?;
                write_escaped_attr(&mut self.writer, &ns.uri)?;
                self.w("\"")?;
            } else {
                self.w(" xmlns:")?;
                self.w(&ns.prefix)?;
                self.w("=\"")?;
                write_escaped_attr(&mut self.writer, &ns.uri)?;
                self.w("\"")?;
            }
        }

        // Attribute schreiben
        for (attr_qname, value) in &attrs {
            self.w(" ")?;
            self.write_resolved_attr_name(attr_qname, &ns_decls)?;
            self.w("=\"")?;
            // xsi:type Clark-Notation → prefix:local
            if &*attr_qname.uri == "http://www.w3.org/2001/XMLSchema-instance"
                && &*attr_qname.local_name == "type"
            {
                let resolved =
                    resolve_clark_to_prefixed(value, &ns_decls, &self.element_stack);
                write_escaped_attr(&mut self.writer, &resolved)?;
            } else {
                write_escaped_attr(&mut self.writer, value)?;
            }
            self.w("\"")?;
        }

        if self_closing {
            self.w("/>")?;
        } else {
            self.w(">")?;
            self.element_stack.push((qname, ns_decls));
        }

        Ok(())
    }

    /// Hilfsfunktion: String als Bytes schreiben.
    #[inline]
    fn w(&mut self, s: &str) -> Result<()> {
        self.writer.write_all(s.as_bytes()).map_err(io_err)
    }

    /// QName als String schreiben (prefix:local oder nur local).
    fn write_qname(&mut self, q: &QName) -> Result<()> {
        match &q.prefix {
            Some(pfx) if !pfx.is_empty() => {
                self.w(pfx)?;
                self.w(":")?;
                self.w(&q.local_name)?;
            }
            _ => {
                self.w(&q.local_name)?;
            }
        }
        Ok(())
    }

    /// Schreibt den aufgelösten Attribut-Namen direkt in den Writer (vermeidet format!-Allokation).
    fn write_resolved_attr_name(
        &mut self,
        qname: &QName,
        current_ns: &[NsContent],
    ) -> crate::Result<()> {
        // 1. Expliziter Prefix am QName — kein Borrow auf self nötig
        if let Some(ref pfx) = qname.prefix {
            if !pfx.is_empty() {
                self.w(pfx)?;
                self.w(":")?;
                return self.w(&qname.local_name);
            }
        }

        // 2. Prefix aus aktuellen NS-Deklarationen — kein Borrow auf self nötig
        if !qname.uri.is_empty() {
            if let Some(pfx) = find_prefix_for_uri(&qname.uri, current_ns) {
                if !pfx.is_empty() {
                    self.w(pfx)?;
                    self.w(":")?;
                    return self.w(&qname.local_name);
                }
            }

            // 3. Element-Stack: Prefix kopieren um Borrow-Konflikt mit self.w() zu vermeiden.
            //    Prefixe sind kurz (1-5 Bytes), deutlich günstiger als format!("{}:{}", pfx, local).
            let stack_pfx = self
                .element_stack
                .iter()
                .rev()
                .find_map(|(_, ns_decls)| {
                    find_prefix_for_uri(&qname.uri, ns_decls).filter(|p| !p.is_empty())
                })
                .map(str::to_owned);

            if let Some(pfx) = stack_pfx {
                self.w(&pfx)?;
                self.w(":")?;
                return self.w(&qname.local_name);
            }
        }

        self.w(&qname.local_name)
    }

    fn has_prefix(&self, prefix: &str, ns_decls: &[NsContent]) -> bool {
        ns_decls.iter().any(|ns| &*ns.prefix == prefix)
            || self
                .element_stack
                .iter()
                .rev()
                .any(|(_, stack_ns)| stack_ns.iter().any(|ns| &*ns.prefix == prefix))
    }

    fn has_prefix_for_uri(&self, uri: &str, ns_decls: &[NsContent]) -> bool {
        ns_decls
            .iter()
            .any(|ns| &*ns.uri == uri && !ns.prefix.is_empty())
            || self.element_stack.iter().rev().any(|(_, stack_ns)| {
                stack_ns
                    .iter()
                    .any(|ns| &*ns.uri == uri && !ns.prefix.is_empty())
            })
    }

    fn synthesize_missing_ns(
        &mut self,
        elem_qname: &QName,
        attrs: &[(Rc<QName>, Rc<str>)],
        ns_decls: &mut Vec<NsContent>,
    ) {
        let mut needed: Vec<(&str, &str)> = Vec::new();

        if let Some(ref pfx) = elem_qname.prefix {
            if !pfx.is_empty() && !elem_qname.uri.is_empty() {
                needed.push((pfx, &elem_qname.uri));
            }
        }

        for (attr_qname, _) in attrs {
            if !attr_qname.uri.is_empty() {
                match &attr_qname.prefix {
                    Some(pfx) if !pfx.is_empty() => {
                        needed.push((pfx, &attr_qname.uri));
                    }
                    _ => {
                        if !self.has_prefix_for_uri(&attr_qname.uri, ns_decls) {
                            let syn_prefix = loop {
                                let candidate = format!("ns{}", self.synthetic_ns_counter);
                                self.synthetic_ns_counter += 1;
                                if !self.has_prefix(&candidate, ns_decls) {
                                    break candidate;
                                }
                            };
                            ns_decls.push(NsContent {
                                uri: attr_qname.uri.clone(),
                                prefix: syn_prefix.into(),
                                local_element_ns: false,
                            });
                        }
                    }
                }
            }
        }

        for (pfx, uri) in needed {
            if self.has_prefix(pfx, ns_decls) {
                continue;
            }
            ns_decls.push(NsContent {
                uri: uri.into(),
                prefix: pfx.into(),
                local_element_ns: false,
            });
        }
    }
}

/// io::Error → Error Konvertierung.
fn io_err(e: std::io::Error) -> Error {
    Error::IoError(e.to_string())
}

/// Findet den Prefix fuer eine URI in einer Liste von NS-Deklarationen.
fn find_prefix_for_uri<'a>(uri: &str, ns_decls: &'a [NsContent]) -> Option<&'a str> {
    ns_decls
        .iter()
        .find(|ns| &*ns.uri == uri)
        .map(|ns| &*ns.prefix)
}

/// XML-Escaping mit memchr3-SIMD: Sucht drei Zeichen gleichzeitig und ersetzt sie.
/// Grosse Bloecke ohne Escape-Zeichen werden in einem Stueck geschrieben.
fn write_escaped_memchr3(
    w: &mut impl Write,
    s: &str,
    needle: [u8; 3],
    replacement: [&[u8]; 3],
) -> Result<()> {
    let bytes = s.as_bytes();
    let mut start = 0;
    while start < bytes.len() {
        match memchr::memchr3(needle[0], needle[1], needle[2], &bytes[start..]) {
            Some(offset) => {
                let pos = start + offset;
                if start < pos {
                    w.write_all(&bytes[start..pos]).map_err(io_err)?;
                }
                let idx = needle.iter().position(|&n| n == bytes[pos]).unwrap();
                w.write_all(replacement[idx]).map_err(io_err)?;
                start = pos + 1;
            }
            None => {
                w.write_all(&bytes[start..]).map_err(io_err)?;
                break;
            }
        }
    }
    Ok(())
}

/// XML-Escaping fuer Text-Inhalt: & < > → &amp; &lt; &gt;
fn write_escaped_text(w: &mut impl Write, s: &str) -> Result<()> {
    write_escaped_memchr3(w, s, [b'&', b'<', b'>'], [b"&amp;", b"&lt;", b"&gt;"])
}

/// XML-Escaping fuer Attribut-Werte: & < " → &amp; &lt; &quot;
fn write_escaped_attr(w: &mut impl Write, s: &str) -> Result<()> {
    write_escaped_memchr3(w, s, [b'&', b'<', b'"'], [b"&amp;", b"&lt;", b"&quot;"])
}

/// Konvertiert Clark-Notation `{URI}local` → `prefix:local` anhand der
/// aktuellen NS-Bindings.
fn resolve_clark_to_prefixed(
    value: &str,
    current_ns: &[NsContent],
    stack: &[(Rc<QName>, Vec<NsContent>)],
) -> String {
    let Some(open) = value.find('{') else {
        return value.to_string();
    };
    let Some(close) = value.find('}') else {
        return value.to_string();
    };
    if open != 0 || close <= 1 {
        return value.to_string();
    }

    let uri = &value[1..close];
    let local = &value[close + 1..];

    if let Some(pfx) = find_prefix_for_uri(uri, current_ns) {
        if !pfx.is_empty() {
            return format!("{}:{}", pfx, local);
        }
    }

    for (_elem_qname, ns_decls) in stack.iter().rev() {
        if let Some(pfx) = find_prefix_for_uri(uri, ns_decls) {
            if !pfx.is_empty() {
                return format!("{}:{}", pfx, local);
            }
        }
    }

    value.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_text_ampersand() {
        let mut buf = Vec::new();
        write_escaped_text(&mut buf, "a&b").unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "a&amp;b");
    }

    #[test]
    fn escape_text_lt_gt() {
        let mut buf = Vec::new();
        write_escaped_text(&mut buf, "a<b>c").unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "a&lt;b&gt;c");
    }

    #[test]
    fn escape_attr_quote() {
        let mut buf = Vec::new();
        write_escaped_attr(&mut buf, r#"a"b"#).unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "a&quot;b");
    }

    #[test]
    fn qname_to_string_mit_prefix() {
        let q = QName::with_prefix("http://a", "x", "a");
        let mut ser = XmlStreamSerializer::new(Vec::new());
        ser.write_qname(&q).unwrap();
        assert_eq!(String::from_utf8(ser.writer).unwrap(), "a:x");
    }

    #[test]
    fn qname_to_string_ohne_prefix() {
        let q = QName::new("", "x");
        let mut ser = XmlStreamSerializer::new(Vec::new());
        ser.write_qname(&q).unwrap();
        assert_eq!(String::from_utf8(ser.writer).unwrap(), "x");
    }

    #[test]
    fn clark_to_prefixed_kein_clark() {
        let result = resolve_clark_to_prefixed("plain", &[], &[]);
        assert_eq!(result, "plain");
    }

    #[test]
    fn clark_to_prefixed_mit_ns() {
        let ns = vec![NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        }];
        let result = resolve_clark_to_prefixed("{http://example.org}MyType", &ns, &[]);
        assert_eq!(result, "ex:MyType");
    }

    #[test]
    fn events_to_xml_writer_roundtrip() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(crate::event::ChContent {
                value: "hello".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let xml_string = events_to_xml(&events).unwrap();
        assert_eq!(xml_string, "<root>hello</root>");

        let mut buf = Vec::new();
        events_to_xml_writer(&events, &mut buf).unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "<root>hello</root>");
    }

    #[test]
    fn events_to_xml_iter_test() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let mut buf = Vec::new();
        events_to_xml_iter(events.into_iter(), &mut buf).unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "<a/>");
    }
}
