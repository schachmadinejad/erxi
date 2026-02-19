use crate::error::Error;
use crate::event::{DtContent, NsContent};
use crate::qname::QName;
use crate::Result;
use crate::FastHashMap;
use memchr::memchr;
use std::borrow::Cow;
use quick_xml::escape::resolve_predefined_entity;
use quick_xml::events::{BytesCData, BytesStart, BytesText, Event};
use quick_xml::name::{ResolveResult, QName as XmlQName};
use quick_xml::reader::NsReader;
use std::path::Path;

use super::{ParseFlags, XmlEvent};
use super::dtd::{parse_dtd_entities, parse_external_entities};

pub(crate) fn emit_xml_events(
    xml_reader: impl std::io::Read,
    flags: &ParseFlags,
    streaming_dtd_guard: bool,
    base_path: Option<&Path>,
    mut emit: impl FnMut(XmlEvent<'_>) -> Result<()>,
) -> Result<()> {
    let mut reader = NsReader::from_reader(std::io::BufReader::new(xml_reader));
    reader.config_mut().trim_text(false);

    let mut buf = Vec::new();
    let mut depth: usize = 0;
    // CH-Coalescing: gepufferter Text wird vor jedem Nicht-CH-Event geflusht.
    let mut pending_ch: Option<String> = None;
    // Entity-Map: aus DOCTYPE geparste Entities (nur im Streaming-Modus aktiv).
    let mut entity_map: FastHashMap<String, String> = FastHashMap::default();
    // Trackt pro Verschachtelungstiefe ob ein Kind-SE gesehen wurde.
    // Nach einem Kind-SE ist reiner Whitespace zwischen Elementen insignifikant
    // und kann beim fruehen Filtern uebersprungen werden.
    let mut had_child_se: Vec<bool> = Vec::new();
    // QName-Pool: cached wiederkehrende QNames (Element/Attribut), vermeidet
    // ~3× Rc<str> Allokation+Drop pro Event bei ~20 einzigartigen QNames.
    let mut qname_pool: FastHashMap<u64, QName> = FastHashMap::with_capacity_and_hasher(32, Default::default());

    // EXI fragment streams still use SD/ED in the Fragment grammar.
    // Keep SD/ED and relax XML well-formedness checks when fragment=true.
    // Spec 8.5.2/8.5.3 (spec/exi-spec.txt lines 2125-2167).
    emit(XmlEvent::StartDocument)?;

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(e)) => {
                flush_pending_ch(&mut pending_ch, &mut emit)?;
                // Geschwister-SE auf aktueller Tiefe markieren
                if let Some(flag) = had_child_se.last_mut() {
                    *flag = true;
                }
                emit_start(
                    &reader,
                    e,
                    flags.preserve_prefixes,
                    flags.preserve_dtd,
                    false,
                    &mut emit,
                    &entity_map,
                    &mut qname_pool,
                )?;
                depth = depth.saturating_add(1);
                had_child_se.push(false);
            }
            Ok(Event::Empty(e)) => {
                flush_pending_ch(&mut pending_ch, &mut emit)?;
                // Empty-Element zaehlt als Geschwister-SE
                if let Some(flag) = had_child_se.last_mut() {
                    *flag = true;
                }
                emit_start(
                    &reader,
                    e,
                    flags.preserve_prefixes,
                    flags.preserve_dtd,
                    true,
                    &mut emit,
                    &entity_map,
                    &mut qname_pool,
                )?;
            }
            Ok(Event::End(_e)) => {
                flush_pending_ch(&mut pending_ch, &mut emit)?;
                had_child_se.pop();
                depth = depth.checked_sub(1).ok_or_else(|| {
                    Error::XmlParseError("unerwartetes End-Element bei depth=0".to_string())
                })?;
                emit(XmlEvent::EndElement)?;
            }
            Ok(Event::Text(e)) => {
                // Insignifikanten Whitespace frueh filtern, VOR decode_text,
                // um String-Allokation zu sparen.
                // Nur wenn kein pending_ch existiert — sonst wuerde Coalescing
                // gebrochen (WS kann an Nicht-WS angrenzen, getrennt durch Comments).
                if flags.strip_whitespace
                    && depth > 0
                    && pending_ch.is_none()
                    && had_child_se.last() == Some(&true)
                    && e.as_ref().iter().all(|&b| matches!(b, b' ' | b'\t' | b'\r' | b'\n'))
                {
                    // Insignifikanter Whitespace: ueberspringen
                } else if let Some(value) = decode_text(e)? {
                    if depth == 0 {
                        if !value.trim().is_empty() {
                            return Err(Error::XmlParseError(
                                "character data outside root element".to_string(),
                            ));
                        }
                    } else {
                        coalesce_ch(&mut pending_ch, value);
                    }
                }
            }
            Ok(Event::CData(e)) => {
                if let Some(value) = decode_cdata(e)? {
                    if depth == 0 {
                        if !value.trim().is_empty() {
                            return Err(Error::XmlParseError(
                                "character data outside root element".to_string(),
                            ));
                        }
                    } else {
                        coalesce_ch(&mut pending_ch, value);
                    }
                }
            }
            Ok(Event::Comment(e)) => {
                if flags.preserve_comments {
                    flush_pending_ch(&mut pending_ch, &mut emit)?;
                    let text = decode_text_bytes(e.as_ref())?;
                    emit(XmlEvent::Comment(&text))?;
                }
                // preserve_comments=false: kein Flush, CH-Coalescing weiter aktiv
            }
            Ok(Event::PI(e)) => {
                if flags.preserve_pis {
                    flush_pending_ch(&mut pending_ch, &mut emit)?;
                    let name = decode_text_bytes(e.target())?;
                    let data = e.content();
                    let text = decode_text_bytes(data)?;
                    // XML Spec 2.6: S zwischen PITarget und Data ist Separator,
                    // nicht Teil der Daten. quick-xml content() schliesst S mit
                    // ein, SAX (Exificient) nicht. Daher leading WS strippen.
                    let text = text.trim_start().to_string();
                    emit(XmlEvent::ProcessingInstruction { target: &name, data: &text })?;
                }
                // preserve_pis=false: kein Flush, CH-Coalescing weiter aktiv
            }
            Ok(Event::DocType(e)) => {
                let raw = decode_text_bytes(e.as_ref())?;
                if streaming_dtd_guard {
                    // Entities aus Internal Subset parsen (falls vorhanden)
                    let fake_xml = format!("<!DOCTYPE{}>", raw);
                    let internal = parse_dtd_entities(&fake_xml);
                    entity_map.extend(internal);

                    // Externe Entity-Deklarationen im Internal Subset
                    // (<!ENTITY name SYSTEM "datei">) brauchen base_path
                    let external = parse_external_entities(&fake_xml);
                    if !external.is_empty() {
                        let Some(bp) = base_path else {
                            return Err(Error::DtdRequiresBatchApi);
                        };
                        for (name, uri) in external {
                            let entity_path = bp.join(&uri);
                            let content = std::fs::read_to_string(&entity_path)
                                .map_err(|e| Error::IoError(format!(
                                    "Externe Entity '{}' nicht lesbar ({}): {e}", name, entity_path.display()
                                )))?;
                            entity_map.insert(name, content);
                        }
                    }
                }
                if flags.preserve_dtd {
                    flush_pending_ch(&mut pending_ch, &mut emit)?;
                    emit(XmlEvent::DocType(parse_doctype_fields(&raw)))?;
                }
                // preserve_dtd=false: kein Flush, CH-Coalescing weiter aktiv
            }
            Ok(Event::GeneralRef(e)) => {
                let ref_name = decode_text_bytes(e.as_ref())?;
                if ref_name.starts_with('#') {
                    if let Some(ch) = resolve_char_reference(&ref_name)
                        && depth > 0 {
                            coalesce_ch(&mut pending_ch, ch.to_string());
                        }
                } else if let Some(resolved) = resolve_predefined_entity(&ref_name) {
                    if depth > 0 {
                        coalesce_ch(&mut pending_ch, resolved.to_string());
                    }
                } else if !flags.preserve_dtd {
                    if let Some(value) = entity_map.get(&ref_name) {
                        // DTD-Entity aus Internal Subset aufgelöst
                        // (nur bei preserve_dtd=false — bei true bleiben Entities als ER-Events)
                        if depth > 0 {
                            coalesce_ch(&mut pending_ch, value.clone());
                        }
                    }
                    // Unbekannte Entity bei preserve_dtd=false: still ignoriert
                    // (Batch-Pfad löst alle Entities vorher auf)
                } else {
                    // preserve_dtd=true: Entity als ER-Event erhalten
                    flush_pending_ch(&mut pending_ch, &mut emit)?;
                    emit(XmlEvent::EntityReference(&ref_name))?;
                }
            }
            Ok(Event::Decl(_d)) => {
                // StartDocument already emitted.
            }
            Ok(Event::Eof) => break,
            Err(e) => {
                return Err(Error::XmlParseError(format!(
                    "parse XML error at {:?}: {e}",
                    reader.buffer_position()
                )));
            }
        }

        buf.clear();
    }

    flush_pending_ch(&mut pending_ch, &mut emit)?;
    emit(XmlEvent::EndDocument)?;
    Ok(())
}

/// Flusht gepufferte CH-Daten als Characters-Event.
fn flush_pending_ch(
    pending_ch: &mut Option<String>,
    emit: &mut impl FnMut(XmlEvent<'_>) -> Result<()>,
) -> Result<()> {
    if let Some(text) = pending_ch.take() {
        emit(XmlEvent::Characters(&text))?;
    }
    Ok(())
}

/// CH-Coalescing: Text an gepufferten CH-Wert anhängen oder neuen starten.
fn coalesce_ch(pending_ch: &mut Option<String>, value: String) {
    match pending_ch {
        Some(existing) => existing.push_str(&value),
        None => *pending_ch = Some(value),
    }
}

/// Erstellt einen QName mit optionalem Prefix.
fn make_qname(uri: &str, local_name: &str, prefix: Option<&str>) -> QName {
    match prefix {
        Some(pfx) => QName::with_prefix(uri, local_name, pfx),
        None => QName::new(uri, local_name),
    }
}

/// Pool-Variante von make_qname: gibt gecachten QName-Referenz zurück oder erzeugt+cached.
/// Vermeidet ~3× Rc<str> Allokation pro Event bei wiederkehrenden Element/Attribut-QNames.
#[inline]
fn pool_qname_ref<'a>(
    pool: &'a mut FastHashMap<u64, QName>,
    uri: &str,
    local_name: &str,
    prefix: Option<&str>,
) -> &'a QName {
    let identity = crate::qname::compute_identity(uri, local_name);
    let qname = pool.entry(identity).or_insert_with(|| make_qname(uri, local_name, prefix));
    debug_assert!(
        &*qname.uri == uri && &*qname.local_name == local_name,
        "pool_qname_ref: Hash-Kollision für ({uri}, {local_name}) vs ({}, {})",
        qname.uri, qname.local_name,
    );
    qname
}

/// Pool-Variante mit Clone für Caller die Ownership brauchen (Attribut-Vec).
#[inline]
fn pool_qname(
    pool: &mut FastHashMap<u64, QName>,
    uri: &str,
    local_name: &str,
    prefix: Option<&str>,
) -> QName {
    pool_qname_ref(pool, uri, local_name, prefix).clone()
}

fn emit_start(
    reader: &NsReader<impl std::io::BufRead>,
    e: BytesStart<'_>,
    preserve_prefixes: bool,
    _preserve_dtd: bool,
    is_empty: bool,
    emit: &mut impl FnMut(XmlEvent<'_>) -> Result<()>,
    entity_map: &FastHashMap<String, String>,
    qname_pool: &mut FastHashMap<u64, QName>,
) -> Result<()> {
    let (elem_uri, elem_local, elem_prefix) = resolve_element_qname(reader, e.name())?;
    // Borrow direkt aus Pool — spart 2-3 Rc::clone pro Element.
    // NLL: Borrow endet nach emit(), Pool ist danach wieder frei für Attribute.
    let elem_qname = pool_qname_ref(
        qname_pool,
        &elem_uri,
        &elem_local,
        if preserve_prefixes { elem_prefix.as_deref() } else { None },
    );

    emit(XmlEvent::StartElement(elem_qname))?;

    let mut ns_decls = Vec::with_capacity(4);
    let mut attrs: Vec<(QName, String)> = Vec::with_capacity(8);

    for attr in e.attributes().with_checks(false) {
        let attr = attr.map_err(|er| Error::XmlParseError(er.to_string()))?;
        let key = attr.key.as_ref();

        if let Some(ns_decl) =
            parse_namespace_decl(key, &attr, &elem_uri, &elem_prefix, entity_map)?
        {
            if preserve_prefixes {
                ns_decls.push(ns_decl);
            }
            continue;
        }

        let (attr_uri, attr_local, attr_prefix) =
            resolve_attribute_qname(reader, attr.key)?;
        let value_raw = std::str::from_utf8(attr.value.as_ref())
            .map_err(|er| Error::XmlParseError(er.to_string()))?;
        let value = unescape_attr_value(value_raw, entity_map);
        let value = normalize_line_endings(value.as_ref()).into_owned();

        let qname = pool_qname(
            qname_pool,
            &attr_uri,
            &attr_local,
            if preserve_prefixes { attr_prefix.as_deref() } else { None },
        );

        // Spec 8.5.4.4: xsi:type-Wert ist ein QName, dessen Prefix hier
        // aufgeloest werden muss, damit der Encoder den korrekten URI kennt.
        let value = if &*qname.uri == "http://www.w3.org/2001/XMLSchema-instance"
            && &*qname.local_name == "type"
        {
            resolve_xsi_type_value(reader, &value)
        } else {
            value
        };

        attrs.push((qname, value));
    }

    // Attribute order is not significant in XML. Sort by QName (local-name, then URI)
    // to match the schema-informed grammar ordering and avoid InvalidEventCode.
    attrs.sort_by(|(qa, _), (qb, _)| {
        qa.local_name
            .cmp(&qb.local_name)
            .then_with(|| qa.uri.cmp(&qb.uri))
    });

    if preserve_prefixes {
        for ns in ns_decls {
            emit(XmlEvent::NamespaceDeclaration(ns))?;
        }
    }
    for (qname, value) in &attrs {
        emit(XmlEvent::Attribute { qname, value })?;
    }

    if is_empty {
        emit(XmlEvent::EndElement)?;
    }

    Ok(())
}

fn resolve_element_qname(
    reader: &NsReader<impl std::io::BufRead>,
    name: XmlQName<'_>,
) -> Result<(String, String, Option<String>)> {
    let (ns, local) = reader.resolver().resolve_element(name);
    let uri = resolve_to_uri(ns)?;
    let local_name = decode_text_bytes(local.as_ref())?;
    let prefix = split_prefix(name.as_ref())
        .and_then(|p| decode_text_bytes(p).ok());
    Ok((uri, local_name, prefix))
}

fn resolve_attribute_qname(
    reader: &NsReader<impl std::io::BufRead>,
    name: XmlQName<'_>,
) -> Result<(String, String, Option<String>)> {
    let (ns, local) = reader.resolver().resolve_attribute(name);
    let uri = resolve_to_uri(ns)?;
    let local_name = decode_text_bytes(local.as_ref())?;
    let prefix = split_prefix(name.as_ref())
        .and_then(|p| decode_text_bytes(p).ok());
    Ok((uri, local_name, prefix))
}

/// Loest den Prefix im xsi:type-Wert auf und gibt das Ergebnis als
/// Clark-Notation `{URI}localname` zurueck. Falls kein Prefix vorhanden
/// oder die Aufloesung fehlschlaegt, wird der Wert unveraendert zurueckgegeben.
fn resolve_xsi_type_value(
    reader: &NsReader<impl std::io::BufRead>,
    value: &str,
) -> String {
    if !value.contains(':') {
        return value.to_string();
    }
    // NsReader::resolve_element loest den Prefix als Element-QName auf.
    let raw = value.as_bytes();
    let (ns, local) = reader.resolver().resolve_element(XmlQName(raw));
    if let ResolveResult::Bound(ns_uri) = ns {
        // SAFETY: quick_xml garantiert UTF-8 — wir verwenden nur UTF-8 Quellen.
        let uri = unsafe { std::str::from_utf8_unchecked(ns_uri.as_ref()) };
        let local_name = unsafe { std::str::from_utf8_unchecked(local.as_ref()) };
        format!("{{{uri}}}{local_name}")
    } else {
        value.to_string()
    }
}

/// Löst DTD-Entities in Attributwerten auf (nur bekannte Entities).
/// Unbekannte Entities bleiben unverändert.
fn unescape_attr_value<'a>(
    value: &'a str,
    entities: &FastHashMap<String, String>,
) -> Cow<'a, str> {
    let bytes = value.as_bytes();
    let Some(mut amp) = memchr(b'&', bytes) else {
        return Cow::Borrowed(value);
    };

    let mut out = String::with_capacity(value.len());
    let mut pos = 0;
    let mut changed = false;

    loop {
        out.push_str(&value[pos..amp]);
        let after = &bytes[amp + 1..];
        let Some(rel_semi) = memchr(b';', after) else {
            out.push_str(&value[amp..]);
            return Cow::Owned(out);
        };
        let semi = amp + 1 + rel_semi;
        let name = &value[amp + 1..semi];
        if name.starts_with('#') {
            if let Some(ch) = resolve_char_reference(name) {
                out.push(ch);
                changed = true;
            } else {
                out.push_str(&value[amp..=semi]);
            }
        } else if let Some(predef) = resolve_predefined_entity(name) {
            out.push_str(predef);
            changed = true;
        } else if let Some(repl) = entities.get(name) {
            out.push_str(repl);
            changed = true;
        } else {
            out.push_str(&value[amp..=semi]);
        }
        pos = semi + 1;
        if pos >= value.len() {
            break;
        }
        match memchr(b'&', &bytes[pos..]) {
            Some(rel) => amp = pos + rel,
            None => {
                out.push_str(&value[pos..]);
                break;
            }
        }
    }

    if changed {
        Cow::Owned(out)
    } else {
        Cow::Borrowed(value)
    }
}

/// XML 1.0 Sec. 2.11: \r\n -> \n, alleinstehende \r -> \n
fn normalize_line_endings<'a>(s: &'a str) -> Cow<'a, str> {
    if memchr(b'\r', s.as_bytes()).is_none() {
        return Cow::Borrowed(s);
    }
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if matches!(chars.peek(), Some('\n')) {
                chars.next();
            }
            out.push('\n');
        } else {
            out.push(ch);
        }
    }
    Cow::Owned(out)
}

fn resolve_to_uri(ns: ResolveResult<'_>) -> Result<String> {
    match ns {
        ResolveResult::Bound(ns) => {
            // SAFETY: quick_xml garantiert UTF-8 — wir verwenden nur UTF-8 Quellen.
            Ok(unsafe { String::from_utf8_unchecked(ns.as_ref().to_vec()) })
        }
        ResolveResult::Unbound => Ok(String::new()),
        ResolveResult::Unknown(_) => Err(Error::XmlParseError(
            "unknown namespace prefix".into(),
        )),
    }
}

fn parse_namespace_decl(
    key: &[u8],
    attr: &quick_xml::events::attributes::Attribute,
    elem_uri: &str,
    elem_prefix: &Option<String>,
    entities: &FastHashMap<String, String>,
) -> Result<Option<NsContent>> {
    let is_default = key == b"xmlns";
    let is_prefixed = key.starts_with(b"xmlns:");
    if !is_default && !is_prefixed {
        return Ok(None);
    }

    let prefix = if is_default {
        ""
    } else {
        let (_, p) = key.split_at(6);
        std::str::from_utf8(p).unwrap_or("")
    };

    let raw = std::str::from_utf8(attr.value.as_ref())
        .map_err(|er| Error::XmlParseError(er.to_string()))?;
    let uri = unescape_attr_value(raw, entities).into_owned();

    let local_element_ns = match elem_prefix {
        Some(pfx) => pfx == prefix && elem_uri == uri,
        None => prefix.is_empty() && elem_uri == uri,
    };

    Ok(Some(NsContent {
        uri: uri.into(),
        prefix: prefix.into(),
        local_element_ns,
    }))
}

fn decode_text(e: BytesText<'_>) -> Result<Option<String>> {
    // SAFETY: quick_xml garantiert UTF-8 — wir verwenden nur UTF-8 Quellen.
    debug_assert!(std::str::from_utf8(&*e).is_ok(), "decode_text: Input war nicht UTF-8");
    let raw = unsafe { std::str::from_utf8_unchecked(&*e) };
    let text = quick_xml::escape::unescape(raw)
        .map_err(|er| Error::XmlParseError(er.to_string()))?;
    if text.is_empty() {
        Ok(None)
    } else {
        Ok(Some(normalize_line_endings(text.as_ref()).into_owned()))
    }
}

fn decode_cdata(e: BytesCData<'_>) -> Result<Option<String>> {
    // SAFETY: quick_xml garantiert UTF-8 — wir verwenden nur UTF-8 Quellen.
    let bytes = e.into_inner().to_vec();
    debug_assert!(std::str::from_utf8(&bytes).is_ok(), "decode_cdata: Input war nicht UTF-8");
    let text = unsafe { String::from_utf8_unchecked(bytes) };
    if text.is_empty() {
        Ok(None)
    } else {
        Ok(Some(normalize_line_endings(&text).into_owned()))
    }
}

fn decode_text_bytes(bytes: &[u8]) -> Result<String> {
    // SAFETY: quick_xml garantiert UTF-8 — wir verwenden nur UTF-8 Quellen.
    let s = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };
    Ok(normalize_line_endings(&s).into_owned())
}

fn split_prefix(name: &[u8]) -> Option<&[u8]> {
    let pos = name.iter().position(|b| *b == b':')?;
    Some(&name[..pos])
}

/// Parst den Inhalt eines DOCTYPE-Events in die 4 Spec-Felder (B.8).
///
/// quick-xml liefert den gesamten Inhalt zwischen `<!DOCTYPE ` und `>`.
/// Beispiele:
///   `foo:P` → name="foo:P"
///   `root SYSTEM "root.dtd"` → name="root", system="root.dtd"
///   `A [ <!ENTITY x "v"> ]` → name="A", text=" <!ENTITY x \"v\"> "
fn parse_doctype_fields(raw: &str) -> DtContent {
    let trimmed = raw.trim();

    // Name: erstes Token (vor Whitespace oder '[')
    let name_end = trimmed
        .find(|c: char| c.is_whitespace() || c == '[')
        .unwrap_or(trimmed.len());
    let name = trimmed[..name_end].to_string();
    let rest = trimmed[name_end..].trim_start();

    let mut public = String::new();
    let mut system = String::new();

    // PUBLIC/SYSTEM Identifier extrahieren
    let rest = if let Some(after) = rest.strip_prefix("SYSTEM") {
        let after = after.trim_start();
        let (sys, remaining) = extract_quoted_string(after);
        system = sys;
        remaining.trim_start()
    } else if let Some(after) = rest.strip_prefix("PUBLIC") {
        let after = after.trim_start();
        let (pub_id, remaining) = extract_quoted_string(after);
        public = pub_id;
        let remaining = remaining.trim_start();
        let (sys, remaining) = extract_quoted_string(remaining);
        system = sys;
        remaining.trim_start()
    } else {
        rest
    };

    // Internal Subset: Deklarationen rekonstruieren (Exificient-Format)
    let mut text = String::new();
    if let Some(open) = rest.find('[') {
        let after_bracket = &rest[open + 1..];
        if let Some(close) = after_bracket.rfind(']') {
            text = reconstruct_internal_subset(&after_bracket[..close]);
        }
    }

    DtContent {
        name: name.into(),
        public: public.into(),
        system: system.into(),
        text: text.into(),
    }
}

/// Extrahiert einen quoted String (einfache oder doppelte Anfuehrungszeichen).
fn extract_quoted_string(s: &str) -> (String, &str) {
    if s.is_empty() {
        return (String::new(), s);
    }
    let quote = s.as_bytes()[0];
    if quote != b'"' && quote != b'\'' {
        return (String::new(), s);
    }
    let q = quote as char;
    if let Some(end) = s[1..].find(q) {
        (s[1..1 + end].to_string(), &s[1 + end + 1..])
    } else {
        (String::new(), s)
    }
}

/// Rekonstruiert das interne DTD-Subset im Exificient-Format.
///
/// Parst Deklarationen aus dem rohen Subset-Text und gibt sie normalisiert
/// zurueck. Externe Entity-Deklarationen (SYSTEM/PUBLIC) werden uebersprungen.
/// Jede Deklaration endet mit `> ` (trailing space, wie Exificient).
fn reconstruct_internal_subset(raw_subset: &str) -> String {
    let mut result = String::new();
    let mut pos = 0;
    let bytes = raw_subset.as_bytes();

    while pos < bytes.len() {
        // Whitespace ueberspringen
        if bytes[pos].is_ascii_whitespace() {
            pos += 1;
            continue;
        }

        // Kommentar: <!--...-->
        if raw_subset[pos..].starts_with("<!--")
            && let Some(end) = raw_subset[pos + 4..].find("-->") {
                let comment = &raw_subset[pos..pos + 4 + end + 3];
                result.push_str(comment);
                pos += 4 + end + 3;
                continue;
            }

        // Processing Instruction: <?target data?>
        if raw_subset[pos..].starts_with("<?")
            && let Some(end) = raw_subset[pos + 2..].find("?>") {
                let pi = &raw_subset[pos..pos + 2 + end + 2];
                result.push_str(pi);
                pos += 2 + end + 2;
                continue;
            }

        // <!ENTITY ...>
        if raw_subset[pos..].starts_with("<!ENTITY")
            && let Some(gt_offset) = raw_subset[pos..].find('>') {
                let decl = &raw_subset[pos..pos + gt_offset + 1];

                // Pruefen ob extern (SYSTEM oder PUBLIC nach dem Entity-Namen)
                let after_entity = decl["<!ENTITY".len()..].trim_start();
                let name_end = after_entity
                    .find(|c: char| c.is_whitespace())
                    .unwrap_or(after_entity.len());
                let entity_name = &after_entity[..name_end];
                let after_name = after_entity[name_end..].trim_start();

                if after_name.starts_with("SYSTEM") || after_name.starts_with("PUBLIC") {
                    // Externe Entity: ueberspringen (wie Exificient)
                    pos += gt_offset + 1;
                    continue;
                }

                // Interne Entity: normalisiert rekonstruieren
                // Format: <!ENTITY name "value">  (mit trailing space)
                if let Some(quote_char) = after_name.chars().next()
                    && (quote_char == '"' || quote_char == '\'') {
                        let value_start = 1;
                        if let Some(value_end) = after_name[value_start..].find(quote_char) {
                            let value = &after_name[value_start..value_start + value_end];
                            result.push_str("<!ENTITY ");
                            result.push_str(entity_name);
                            result.push_str(" \"");
                            result.push_str(value);
                            result.push_str("\"> ");
                            pos += gt_offset + 1;
                            continue;
                        }
                    }

                // Fallback: Deklaration roh uebernehmen
                result.push_str(decl);
                result.push(' ');
                pos += gt_offset + 1;
                continue;
            }

        // <!ELEMENT ...>, <!ATTLIST ...>, <!NOTATION ...>
        if let Some(advance) = reconstruct_simple_decl(raw_subset, pos, &mut result) {
            pos += advance;
            continue;
        }

        // Unbekanntes Zeichen: ueberspringen
        pos += 1;
    }

    result
}

/// Versucht eine einfache DTD-Deklaration (ELEMENT, ATTLIST, NOTATION) ab `pos`
/// zu rekonstruieren. Gibt die Anzahl zu ueberspringender Bytes zurueck, oder
/// `None` wenn keine passende Deklaration gefunden wurde.
const SIMPLE_DECL_KEYWORDS: [&str; 3] = ["<!ELEMENT", "<!ATTLIST", "<!NOTATION"];

fn reconstruct_simple_decl(raw_subset: &str, pos: usize, result: &mut String) -> Option<usize> {
    let rest = &raw_subset[pos..];
    let keyword = SIMPLE_DECL_KEYWORDS.iter().find(|kw| rest.starts_with(**kw))?;
    let gt_offset = rest.find('>')?;
    let decl = &rest[..gt_offset + 1];
    let inner = decl[keyword.len()..].trim();
    let inner = inner.strip_suffix('>').unwrap_or(inner).trim();
    result.push_str(keyword);
    result.push(' ');
    result.push_str(inner);
    result.push_str("> ");
    Some(gt_offset + 1)
}

/// Loest eine XML-Zeichenreferenz auf.
///
/// Eingabe: `#49` (dezimal) oder `#x31` (hexadezimal), ohne `&` und `;`.
fn resolve_char_reference(ref_name: &str) -> Option<char> {
    let digits = &ref_name[1..]; // '#' ueberspringen
    let code_point = if let Some(hex) = digits.strip_prefix('x') {
        u32::from_str_radix(hex, 16).ok()?
    } else {
        digits.parse::<u32>().ok()?
    };
    char::from_u32(code_point)
}
