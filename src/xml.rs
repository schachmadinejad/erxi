//! XML parsing to EXI events.
//!
//! Uses quick-xml to build an EXI event sequence for encoding.

use crate::error::Error;
use crate::event::{
    AtContent, ChContent, CmContent, DtContent, ErContent, ExiEvent, NsContent, PiContent,
};
use crate::options::ExiOptions;
use crate::qname::QName;
use crate::Result;
use std::path::Path;
use std::rc::Rc;

mod dtd;
mod emit;
pub(crate) use emit::emit_xml_events;

/// Internes Event für XML→Encoder Streaming. Borrowed, kein Rc.
///
/// Wird von `emit_xml_events` erzeugt und direkt an den Encoder übergeben.
/// Spart Rc-Allokationen auf dem heißen Streaming-Pfad.
pub(crate) enum XmlEvent<'a> {
    StartDocument,
    EndDocument,
    StartElement(&'a QName),
    EndElement,
    Attribute { qname: &'a QName, value: &'a str },
    Characters(&'a str),
    NamespaceDeclaration(NsContent),
    Comment(&'a str),
    ProcessingInstruction { target: &'a str, data: &'a str },
    DocType(DtContent),
    EntityReference(&'a str),
}

impl<'a> XmlEvent<'a> {
    /// Konvertiert zu ExiEvent (mit Rc-Allokationen).
    ///
    /// Für Aufrufer die ExiEvent brauchen (Batch-API, Callback-Encode).
    pub(crate) fn to_exi_event(&self) -> ExiEvent {
        match self {
            XmlEvent::StartDocument => ExiEvent::StartDocument,
            XmlEvent::EndDocument => ExiEvent::EndDocument,
            XmlEvent::StartElement(q) => ExiEvent::StartElement(Rc::new((*q).clone())),
            XmlEvent::EndElement => ExiEvent::EndElement,
            XmlEvent::Attribute { qname, value } => ExiEvent::Attribute(AtContent {
                qname: Rc::new((*qname).clone()),
                value: Rc::from(*value),
            }),
            XmlEvent::Characters(s) => ExiEvent::Characters(ChContent { value: Rc::from(*s) }),
            XmlEvent::NamespaceDeclaration(ns) => ExiEvent::NamespaceDeclaration(ns.clone()),
            XmlEvent::Comment(s) => ExiEvent::Comment(CmContent { text: Rc::from(*s) }),
            XmlEvent::ProcessingInstruction { target, data } => {
                ExiEvent::ProcessingInstruction(PiContent {
                    name: Rc::from(*target),
                    text: Rc::from(*data),
                })
            }
            XmlEvent::DocType(dt) => ExiEvent::DocType(dt.clone()),
            XmlEvent::EntityReference(s) => {
                ExiEvent::EntityReference(ErContent { name: Rc::from(*s) })
            }
        }
    }
}

/// Parsing-Flags, abgeleitet aus ExiOptions.
pub(crate) struct ParseFlags {
    preserve_prefixes: bool,
    preserve_comments: bool,
    preserve_pis: bool,
    preserve_dtd: bool,
    /// Insignifikanten Whitespace zwischen Elementen frueh filtern.
    strip_whitespace: bool,
}

impl ParseFlags {
    pub(crate) fn from_options(opts: &ExiOptions) -> Self {
        let strip_whitespace = !opts.preserve.preserves_whitespace();
        Self {
            preserve_prefixes: opts.preserve.prefixes,
            preserve_comments: opts.preserve.comments,
            preserve_pis: opts.preserve.pis,
            preserve_dtd: opts.preserve.dtd,
            strip_whitespace,
        }
    }

    fn all_preserved(preserve_prefixes: bool) -> Self {
        Self {
            preserve_prefixes,
            preserve_comments: true,
            preserve_pis: true,
            preserve_dtd: true,
            strip_whitespace: false,
        }
    }
}

/// Parse an XML file into EXI events.
///
/// This keeps all XML constructs (comments, PIs, DTD) regardless of EXI options.
pub fn parse_xml_events(path: &Path, preserve_prefixes: bool) -> Result<Vec<ExiEvent>> {
    let flags = ParseFlags::all_preserved(preserve_prefixes);
    parse_xml_events_from_file(path, &flags)
}

/// Parse an XML file into EXI events using EXI options to filter preserved constructs.
pub fn parse_xml_events_with_options(path: &Path, opts: &ExiOptions) -> Result<Vec<ExiEvent>> {
    let flags = ParseFlags::from_options(opts);
    parse_xml_events_from_file(path, &flags)
}

/// Parse XML from a string into EXI events using EXI options.
///
/// DTD-Entity-Aufloesung (intern und extern) wird nicht durchgefuehrt,
/// da kein Dateipfad fuer relative Aufloesung vorliegt.
pub fn parse_xml_events_from_str(xml: &str, opts: &ExiOptions) -> Result<Vec<ExiEvent>> {
    let flags = ParseFlags::from_options(opts);
    // XML 1.0 Sec. 2.11: Zeilenumbruch-Normalisierung
    let normalized = normalize_line_endings(xml);
    parse_xml_events_core(&normalized, &flags)
}

fn parse_xml_events_from_file(path: &Path, flags: &ParseFlags) -> Result<Vec<ExiEvent>> {
    let file = std::fs::File::open(path)
        .map_err(|e| Error::XmlParseError(format!("read XML: {e}")))?;
    let mut events = Vec::new();
    emit_xml_events(
        file,
        flags,
        true,
        path.parent(),
        |xml_event| {
            events.push(xml_event.to_exi_event());
            Ok(())
        },
    )?;
    Ok(events)
}

/// Parst eine XML-Datei und ruft `emit` für jedes Event auf (Callback-basiert, Streaming).
///
/// Liest die Datei über BufRead statt read_to_string() — konstanter Speicherverbrauch
/// auch bei GB-großen Dateien. Bei DTD-Entities gibt `DtdRequiresBatchApi` zurück;
/// der Caller kann dann auf `emit_xml_events_from_file_batch_cb()` fallen.
pub fn emit_xml_events_from_file_cb(
    path: &Path,
    opts: &ExiOptions,
    mut emit: impl FnMut(ExiEvent) -> Result<()>,
) -> Result<()> {
    let flags = ParseFlags::from_options(opts);
    let file = std::fs::File::open(path)
        .map_err(|e| Error::XmlParseError(format!("read XML: {e}")))?;
    emit_xml_events(
        file,
        &flags,
        true,
        path.parent(),
        |xml_event| emit(xml_event.to_exi_event()),
    )
}

/// Parst eine XML-Datei (Batch-Callback).
///
/// Nutzt denselben Streaming-Parser wie der Callback-Pfad, aber liefert
/// `ExiEvent` (Ownership) statt `XmlEvent` (Borrowed).
pub fn emit_xml_events_from_file_batch_cb(
    path: &Path,
    opts: &ExiOptions,
    mut emit: impl FnMut(ExiEvent) -> Result<()>,
) -> Result<()> {
    let flags = ParseFlags::from_options(opts);
    emit_xml_events(
        std::fs::File::open(path)
            .map_err(|e| Error::XmlParseError(format!("read XML: {e}")))?,
        &flags,
        true,
        path.parent(),
        |xml_event| emit(xml_event.to_exi_event()),
    )
}

/// Parst XML aus einem String und ruft `emit` für jedes Event auf (Callback-basiert).
///
/// Analog zu `parse_xml_events_from_str`, aber ohne Vec-Allokation.
pub fn emit_xml_events_from_str_cb(
    xml: &str,
    opts: &ExiOptions,
    mut emit: impl FnMut(ExiEvent) -> Result<()>,
) -> Result<()> {
    let flags = ParseFlags::from_options(opts);
    let normalized = normalize_line_endings(xml);
    emit_xml_events(
        std::io::Cursor::new(normalized.as_bytes()),
        &flags,
        false,
        None,
        |xml_event| emit(xml_event.to_exi_event()),
    )
}

/// XML 1.0 Sec. 2.11: \r\n -> \n, alleinstehende \r -> \n
fn normalize_line_endings(s: &str) -> String {
    s.replace("\r\n", "\n").replace('\r', "\n")
}

fn parse_xml_events_core(xml_string: &str, flags: &ParseFlags) -> Result<Vec<ExiEvent>> {
    let mut events = Vec::new();
    emit_xml_events(
        std::io::Cursor::new(xml_string.as_bytes()),
        flags,
        false,
        None,
        |xml_event| {
            events.push(xml_event.to_exi_event());
            Ok(())
        },
    )?;
    Ok(events)
}
