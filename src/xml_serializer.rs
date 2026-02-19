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
//! - `events_to_pretty_xml*()` — wie oben, aber mit Einrueckung (2 Spaces).

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

/// Serialisiert EXI-Events als pretty-printed XML-String (2 Spaces Einzug).
pub fn events_to_pretty_xml(events: &[ExiEvent]) -> Result<String> {
    let mut buf = Vec::new();
    events_to_pretty_xml_writer(events, &mut buf)?;
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

/// Serialisiert EXI-Events direkt in einen Writer (pretty-printed, 2 Spaces Einzug).
pub fn events_to_pretty_xml_writer(events: &[ExiEvent], writer: impl Write) -> Result<()> {
    let mut ser = PrettyXmlSerializer::new(writer, 2);
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

/// Wie `events_to_xml_iter_fallible`, aber pretty-printed (2 Spaces Einzug).
pub fn events_to_pretty_xml_iter_fallible(
    events: impl Iterator<Item = Result<ExiEvent>>,
    writer: impl Write,
) -> Result<()> {
    let mut ser = PrettyXmlSerializer::new(writer, 2);
    for event_result in events {
        ser.process(&event_result?)?;
    }
    ser.finish()
}

// ============================================================================
// Gemeinsame freie Hilfsfunktionen fuer beide Serializer
// ============================================================================

/// io::Error → Error Konvertierung.
fn io_err(e: std::io::Error) -> Error {
    Error::IoError(e.to_string())
}

/// Schreibt einen String als Bytes in den Writer.
#[inline]
fn w(writer: &mut impl Write, s: &str) -> Result<()> {
    writer.write_all(s.as_bytes()).map_err(io_err)
}

/// Findet den Prefix fuer eine URI in einer Liste von NS-Deklarationen.
fn find_prefix_for_uri<'a>(uri: &str, ns_decls: &'a [NsContent]) -> Option<&'a str> {
    ns_decls
        .iter()
        .find(|ns| &*ns.uri == uri)
        .map(|ns| &*ns.prefix)
}

/// Prueft ob ein Prefix in den NS-Deklarationen oder im Stack vorkommt.
fn has_prefix<'a>(
    prefix: &str,
    ns_decls: &[NsContent],
    mut stack_ns_iter: impl Iterator<Item = &'a [NsContent]>,
) -> bool {
    ns_decls.iter().any(|ns| &*ns.prefix == prefix)
        || stack_ns_iter.any(|stack_ns| stack_ns.iter().any(|ns| &*ns.prefix == prefix))
}

/// Prueft ob eine URI einen (nicht-leeren) Prefix im NS-Stack hat.
fn has_prefix_for_uri<'a>(
    uri: &str,
    ns_decls: &[NsContent],
    mut stack_ns_iter: impl Iterator<Item = &'a [NsContent]>,
) -> bool {
    ns_decls
        .iter()
        .any(|ns| &*ns.uri == uri && !ns.prefix.is_empty())
        || stack_ns_iter
            .any(|stack_ns| stack_ns.iter().any(|ns| &*ns.uri == uri && !ns.prefix.is_empty()))
}

/// Findet einen Prefix fuer eine URI im Element-Stack (Rueckwaerts-Suche).
fn find_prefix_in_stack<'a>(
    uri: &str,
    mut stack_ns_iter: impl Iterator<Item = &'a [NsContent]>,
) -> Option<String> {
    stack_ns_iter
        .find_map(|ns_decls| find_prefix_for_uri(uri, ns_decls).filter(|p| !p.is_empty()))
        .map(str::to_owned)
}

/// Konvertiert Clark-Notation `{URI}local` → `prefix:local` anhand der
/// aktuellen NS-Bindings.
fn resolve_clark_to_prefixed<'a>(
    value: &str,
    current_ns: &[NsContent],
    stack_ns_iter: impl Iterator<Item = &'a [NsContent]>,
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

    for ns_decls in stack_ns_iter {
        if let Some(pfx) = find_prefix_for_uri(uri, ns_decls) {
            if !pfx.is_empty() {
                return format!("{}:{}", pfx, local);
            }
        }
    }

    value.to_string()
}

/// QName als String schreiben (prefix:local oder nur local).
fn write_qname(writer: &mut impl Write, q: &QName) -> Result<()> {
    match &q.prefix {
        Some(pfx) if !pfx.is_empty() => {
            w(writer, pfx)?;
            w(writer, ":")?;
            w(writer, &q.local_name)?;
        }
        _ => {
            w(writer, &q.local_name)?;
        }
    }
    Ok(())
}

/// Schreibt den aufgelösten Attribut-Namen direkt in den Writer (vermeidet format!-Allokation).
fn write_resolved_attr_name<'a>(
    writer: &mut impl Write,
    qname: &QName,
    current_ns: &[NsContent],
    stack_ns_iter: impl Iterator<Item = &'a [NsContent]>,
) -> Result<()> {
    // 1. Expliziter Prefix am QName
    if let Some(ref pfx) = qname.prefix {
        if !pfx.is_empty() {
            w(writer, pfx)?;
            w(writer, ":")?;
            return w(writer, &qname.local_name);
        }
    }

    // 2. Prefix aus aktuellen NS-Deklarationen
    if !qname.uri.is_empty() {
        if let Some(pfx) = find_prefix_for_uri(&qname.uri, current_ns) {
            if !pfx.is_empty() {
                w(writer, pfx)?;
                w(writer, ":")?;
                return w(writer, &qname.local_name);
            }
        }

        // 3. Element-Stack: Prefix kopieren um Borrow-Konflikt mit writer zu vermeiden.
        //    Prefixe sind kurz (1-5 Bytes), deutlich guenstiger als format!("{}:{}", pfx, local).
        if let Some(pfx) = find_prefix_in_stack(&qname.uri, stack_ns_iter) {
            w(writer, &pfx)?;
            w(writer, ":")?;
            return w(writer, &qname.local_name);
        }
    }

    w(writer, &qname.local_name)
}

/// Synthetisiert fehlende NS-Deklarationen fuer Element und Attribute.
fn synthesize_missing_ns<'a>(
    elem_qname: &QName,
    attrs: &[(Rc<QName>, Rc<str>)],
    ns_decls: &mut Vec<NsContent>,
    synthetic_ns_counter: &mut usize,
    stack_ns_iter_fn: impl Fn() -> Box<dyn Iterator<Item = &'a [NsContent]> + 'a>,
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
                    if !has_prefix_for_uri(&attr_qname.uri, ns_decls, stack_ns_iter_fn()) {
                        let syn_prefix = loop {
                            let candidate = format!("ns{}", *synthetic_ns_counter);
                            *synthetic_ns_counter += 1;
                            if !has_prefix(&candidate, ns_decls, stack_ns_iter_fn()) {
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
        if has_prefix(pfx, ns_decls, stack_ns_iter_fn()) {
            continue;
        }
        ns_decls.push(NsContent {
            uri: uri.into(),
            prefix: pfx.into(),
            local_element_ns: false,
        });
    }
}

/// Schreibt NS-Deklarationen in den Writer.
fn write_ns_decls(writer: &mut impl Write, ns_decls: &[NsContent]) -> Result<()> {
    for ns in ns_decls {
        if ns.prefix.is_empty() {
            w(writer, " xmlns=\"")?;
            write_escaped_attr(writer, &ns.uri)?;
            w(writer, "\"")?;
        } else {
            w(writer, " xmlns:")?;
            w(writer, &ns.prefix)?;
            w(writer, "=\"")?;
            write_escaped_attr(writer, &ns.uri)?;
            w(writer, "\"")?;
        }
    }
    Ok(())
}

/// Schreibt Attribute in den Writer.
fn write_attrs<'a>(
    writer: &mut impl Write,
    attrs: &[(Rc<QName>, Rc<str>)],
    ns_decls: &[NsContent],
    stack_ns_iter_fn: impl Fn() -> Box<dyn Iterator<Item = &'a [NsContent]> + 'a>,
) -> Result<()> {
    for (attr_qname, value) in attrs {
        w(writer, " ")?;
        write_resolved_attr_name(writer, attr_qname, ns_decls, stack_ns_iter_fn())?;
        w(writer, "=\"")?;
        // xsi:type Clark-Notation → prefix:local
        if &*attr_qname.uri == "http://www.w3.org/2001/XMLSchema-instance"
            && &*attr_qname.local_name == "type"
        {
            let resolved = resolve_clark_to_prefixed(value, ns_decls, stack_ns_iter_fn());
            write_escaped_attr(writer, &resolved)?;
        } else {
            write_escaped_attr(writer, value)?;
        }
        w(writer, "\"")?;
    }
    Ok(())
}

/// Schreibt einen Kommentar, prueft auf XML-Restriktionen.
fn write_comment(writer: &mut impl Write, text: &str) -> Result<()> {
    if text.contains("--") || text.ends_with('-') {
        return Err(Error::XmlParseError(
            "Kommentar-Text enthaelt '--' oder endet mit '-' (XML 1.0 Section 2.5)".into(),
        ));
    }
    w(writer, "<!--")?;
    w(writer, text)?;
    w(writer, "-->")
}

/// Schreibt eine Processing Instruction, prueft auf XML-Restriktionen.
fn write_pi(writer: &mut impl Write, name: &str, text: &str) -> Result<()> {
    if text.contains("?>") {
        return Err(Error::XmlParseError(
            "PI-Data enthaelt '?>' (XML 1.0 Section 2.6)".into(),
        ));
    }
    w(writer, "<?")?;
    w(writer, name)?;
    if !text.is_empty() {
        w(writer, " ")?;
        w(writer, text)?;
    }
    w(writer, "?>")
}

/// Schreibt eine DOCTYPE-Deklaration.
fn write_doctype(
    writer: &mut impl Write,
    name: &str,
    public: &str,
    system: &str,
    text: &str,
) -> Result<()> {
    w(writer, "<!DOCTYPE ")?;
    w(writer, name)?;
    if !public.is_empty() {
        w(writer, " PUBLIC \"")?;
        write_escaped_attr(writer, public)?;
        w(writer, "\" \"")?;
        write_escaped_attr(writer, system)?;
        w(writer, "\"")?;
    } else if !system.is_empty() {
        w(writer, " SYSTEM \"")?;
        write_escaped_attr(writer, system)?;
        w(writer, "\"")?;
    }
    if !text.is_empty() {
        w(writer, " [")?;
        w(writer, text)?;
        w(writer, "]")?;
    }
    w(writer, ">")
}

/// Schreibt eine Entity-Referenz.
fn write_entity_ref(writer: &mut impl Write, name: &str) -> Result<()> {
    w(writer, "&")?;
    w(writer, name)?;
    w(writer, ";")
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

// ============================================================================
// XmlStreamSerializer (kompakter Serializer ohne Einrueckung)
// ============================================================================

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
                    w(&mut self.writer, "</")?;
                    write_qname(&mut self.writer, &qname)?;
                    w(&mut self.writer, ">")?;
                }
            }
            ExiEvent::Characters(ch) => {
                self.flush_pending_start(false)?;
                write_escaped_text(&mut self.writer, &ch.value)?;
            }
            ExiEvent::Comment(cm) => {
                self.flush_pending_start(false)?;
                write_comment(&mut self.writer, &cm.text)?;
            }
            ExiEvent::ProcessingInstruction(pi) => {
                self.flush_pending_start(false)?;
                write_pi(&mut self.writer, &pi.name, &pi.text)?;
            }
            ExiEvent::DocType(dt) => {
                self.flush_pending_start(false)?;
                write_doctype(&mut self.writer, &dt.name, &dt.public, &dt.system, &dt.text)?;
            }
            ExiEvent::EntityReference(er) => {
                self.flush_pending_start(false)?;
                write_entity_ref(&mut self.writer, &er.name)?;
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

        {
            let stack = &self.element_stack;
            let counter = &mut self.synthetic_ns_counter;
            synthesize_missing_ns(
                &qname,
                &attrs,
                &mut ns_decls,
                counter,
                || Box::new(stack.iter().rev().map(|(_, ns)| ns.as_slice())),
            );
        }

        w(&mut self.writer, "<")?;
        write_qname(&mut self.writer, &qname)?;
        write_ns_decls(&mut self.writer, &ns_decls)?;
        write_attrs(
            &mut self.writer,
            &attrs,
            &ns_decls,
            || Box::new(self.element_stack.iter().rev().map(|(_, ns)| ns.as_slice())),
        )?;

        if self_closing {
            w(&mut self.writer, "/>")?;
        } else {
            w(&mut self.writer, ">")?;
            self.element_stack.push((qname, ns_decls));
        }

        Ok(())
    }

    /// QName als String schreiben (fuer Tests zugaenglich).
    #[cfg(test)]
    fn write_qname(&mut self, q: &QName) -> Result<()> {
        write_qname(&mut self.writer, q)
    }
}

// ============================================================================
// PrettyXmlSerializer (Eingerueckter Serializer)
// ============================================================================

#[derive(Clone)]
struct PrettyElemState {
    qname: Rc<QName>,
    ns_decls: Vec<NsContent>,
    has_text: bool,
    has_child: bool,
}

/// Pretty-printing XML-Serializer — schreibt direkt in `W: Write`.
pub(crate) struct PrettyXmlSerializer<W: Write> {
    writer: W,
    element_stack: Vec<PrettyElemState>,
    pending_start: Option<Rc<QName>>,
    pending_ns: Vec<NsContent>,
    pending_attrs: Vec<(Rc<QName>, Rc<str>)>,
    synthetic_ns_counter: usize,
    indent: usize,
    pending_indent: bool,
    last_was_text: bool,
}

impl<W: Write> PrettyXmlSerializer<W> {
    pub fn new(writer: W, indent: usize) -> Self {
        Self {
            writer,
            element_stack: Vec::new(),
            pending_start: None,
            pending_ns: Vec::new(),
            pending_attrs: Vec::new(),
            synthetic_ns_counter: 0,
            indent,
            pending_indent: false,
            last_was_text: false,
        }
    }

    /// Schreibt Newline + Einrueckung fuer Pretty-Printing.
    fn before_child_node(&mut self) -> Result<()> {
        if !self.element_stack.is_empty() && !self.last_was_text {
            self.write_indent(self.element_stack.len())?;
        }
        if let Some(curr) = self.element_stack.last_mut() {
            curr.has_child = true;
        }
        Ok(())
    }

    pub fn process(&mut self, event: &ExiEvent) -> Result<()> {
        match event {
            ExiEvent::StartDocument => {}
            ExiEvent::EndDocument => {
                self.flush_pending_start(false)?;
            }
            ExiEvent::StartElement(qname) => {
                self.flush_pending_start(false)?;
                self.pending_indent = !self.element_stack.is_empty() && !self.last_was_text;
                if let Some(parent) = self.element_stack.last_mut() {
                    parent.has_child = true;
                }
                self.pending_start = Some(qname.clone());
                self.pending_ns.clear();
                self.pending_attrs.clear();
                self.last_was_text = false;
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
                    let elem = self.element_stack.pop().ok_or_else(|| {
                        Error::XmlParseError("EndElement ohne offenes Element".into())
                    })?;
                    if elem.has_child && !elem.has_text {
                        self.write_indent(self.element_stack.len())?;
                    }
                    w(&mut self.writer, "</")?;
                    write_qname(&mut self.writer, &elem.qname)?;
                    w(&mut self.writer, ">")?;
                }
                self.last_was_text = false;
            }
            ExiEvent::Characters(ch) => {
                self.flush_pending_start(false)?;
                if let Some(curr) = self.element_stack.last_mut() {
                    curr.has_text = true;
                }
                write_escaped_text(&mut self.writer, &ch.value)?;
                self.last_was_text = true;
            }
            ExiEvent::Comment(cm) => {
                self.flush_pending_start(false)?;
                self.before_child_node()?;
                write_comment(&mut self.writer, &cm.text)?;
                self.last_was_text = false;
            }
            ExiEvent::ProcessingInstruction(pi) => {
                self.flush_pending_start(false)?;
                self.before_child_node()?;
                write_pi(&mut self.writer, &pi.name, &pi.text)?;
                self.last_was_text = false;
            }
            ExiEvent::DocType(dt) => {
                self.flush_pending_start(false)?;
                self.before_child_node()?;
                write_doctype(&mut self.writer, &dt.name, &dt.public, &dt.system, &dt.text)?;
                self.last_was_text = false;
            }
            ExiEvent::EntityReference(er) => {
                self.flush_pending_start(false)?;
                self.before_child_node()?;
                write_entity_ref(&mut self.writer, &er.name)?;
                self.last_was_text = false;
            }
            ExiEvent::SelfContained => {}
        }
        Ok(())
    }

    pub fn finish(&mut self) -> Result<()> {
        self.flush_pending_start(false)?;
        self.writer.flush().map_err(io_err)?;
        Ok(())
    }

    fn write_indent(&mut self, depth: usize) -> Result<()> {
        if depth == 0 {
            return Ok(());
        }
        // Statischer Spaces-Buffer — deckt bis 128 Spaces ab (depth 64 bei indent=2).
        // Tiefere Verschachtelungen werden in Stuecken geschrieben.
        const SPACES: &[u8; 128] = &[b' '; 128];
        w(&mut self.writer, "\n")?;
        let mut remaining = self.indent * depth;
        while remaining > 0 {
            let chunk = remaining.min(SPACES.len());
            self.writer.write_all(&SPACES[..chunk]).map_err(io_err)?;
            remaining -= chunk;
        }
        Ok(())
    }

    /// Schreibt den gepufferten Start-Tag und pusht das Element auf den Stack.
    fn flush_pending_start(&mut self, self_closing: bool) -> Result<()> {
        let Some(qname) = self.pending_start.take() else {
            return Ok(());
        };

        let mut ns_decls = std::mem::take(&mut self.pending_ns);
        let attrs = std::mem::take(&mut self.pending_attrs);

        {
            let stack = &self.element_stack;
            let counter = &mut self.synthetic_ns_counter;
            synthesize_missing_ns(
                &qname,
                &attrs,
                &mut ns_decls,
                counter,
                || Box::new(stack.iter().rev().map(|elem| elem.ns_decls.as_slice())),
            );
        }

        if self.pending_indent {
            self.write_indent(self.element_stack.len())?;
        }
        self.pending_indent = false;

        w(&mut self.writer, "<")?;
        write_qname(&mut self.writer, &qname)?;
        write_ns_decls(&mut self.writer, &ns_decls)?;
        write_attrs(
            &mut self.writer,
            &attrs,
            &ns_decls,
            || Box::new(self.element_stack.iter().rev().map(|elem| elem.ns_decls.as_slice())),
        )?;

        if self_closing {
            w(&mut self.writer, "/>")?;
        } else {
            w(&mut self.writer, ">")?;
            self.element_stack.push(PrettyElemState {
                qname,
                ns_decls,
                has_text: false,
                has_child: false,
            });
        }

        Ok(())
    }
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
        let empty: &[(&[NsContent],)] = &[];
        let result = resolve_clark_to_prefixed("plain", &[], empty.iter().map(|_| &[][..]));
        assert_eq!(result, "plain");
    }

    #[test]
    fn clark_to_prefixed_mit_ns() {
        let ns = vec![NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        }];
        let empty: Vec<Vec<NsContent>> = vec![];
        let result = resolve_clark_to_prefixed(
            "{http://example.org}MyType",
            &ns,
            empty.iter().map(|v| v.as_slice()),
        );
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
