// Gemeinsame Kanonisierungs- und Normalisierungsfunktionen fuer Cross-RTT-Tests.
//
// Wird per `include!` eingebunden. Benötigte Imports:
//   use erxi::event::ExiEvent;

/// Einfache XML-Normalisierung: Whitespace normalisieren, XML-Declaration entfernen.
fn normalize_xml(text: &str) -> String {
    let mut s = text.to_string();

    // XML-Declaration entfernen
    if let Some(end) = s.find("?>") {
        if s.trim_start().starts_with("<?xml") {
            s = s[end + 2..].to_string();
        }
    }

    // Zeilenumbrüche normalisieren
    s = s.replace("\r\n", "\n").replace('\r', "\n");

    // Führende/trailing Whitespace pro Zeile entfernen, leere Zeilen entfernen
    s.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Serialisiert Events zu kanonischem Text fuer den Vergleich.
///
/// Normalisierungen:
/// - Prefixe entfernt (EXI kann Prefixe aendern)
/// - Attribute lexikographisch nach {uri}local_name sortiert
/// - Whitespace-only CH Events uebersprungen
/// - NS-Deklarationen uebersprungen
/// - xsi:nil="false" gedroppt (Default-Wert, semantisch bedeutungslos)
fn events_to_canonical_ex(events: &[ExiEvent], strip_uris: bool) -> String {
    let mut lines = Vec::new();
    let mut i = 0;

    while i < events.len() {
        match &events[i] {
            ExiEvent::StartDocument => {
                lines.push("SD".to_string());
                i += 1;
            }
            ExiEvent::EndDocument => {
                lines.push("ED".to_string());
                i += 1;
            }
            ExiEvent::StartElement(q) => {
                // SE + nachfolgende Attribute sammeln und sortieren
                let mut attrs = Vec::new();
                i += 1;
                while i < events.len() {
                    if let ExiEvent::Attribute(at) = &events[i] {
                        attrs.push(at.clone());
                        i += 1;
                    } else {
                        break;
                    }
                }
                // xsi:nil='false' droppen (Default-Wert, semantisch bedeutungslos)
                attrs.retain(|at| {
                    !(&*at.qname.uri == "http://www.w3.org/2001/XMLSchema-instance"
                        && &*at.qname.local_name == "nil"
                        && (&*at.value == "false" || &*at.value == "0"))
                });
                if strip_uris {
                    attrs.sort_by(|a, b| a.qname.local_name.cmp(&b.qname.local_name));
                } else {
                    attrs.sort_by(|a, b| {
                        (&a.qname.uri, &a.qname.local_name)
                            .cmp(&(&b.qname.uri, &b.qname.local_name))
                    });
                }
                let mut line = format!("SE({})", format_qname_ex(q, strip_uris));
                for at in &attrs {
                    let value = normalize_at_value(&at.qname, &at.value);
                    line.push_str(&format!(
                        " AT({})=\"{}\"",
                        format_qname_ex(&at.qname, strip_uris),
                        value
                    ));
                }
                lines.push(line);
            }
            ExiEvent::Attribute(_) => {
                i += 1;
            }
            ExiEvent::EndElement => {
                lines.push("EE".to_string());
                i += 1;
            }
            ExiEvent::Characters(ch) => {
                if !ch.value.trim().is_empty() {
                    lines.push(format!("CH(\"{}\")", &ch.value));
                }
                i += 1;
            }
            ExiEvent::NamespaceDeclaration(_) => {
                i += 1;
            }
            ExiEvent::Comment(cm) => {
                lines.push(format!("CM(\"{}\")", &cm.text));
                i += 1;
            }
            ExiEvent::ProcessingInstruction(pi) => {
                lines.push(format!("PI({} \"{}\")", &pi.name, &pi.text));
                i += 1;
            }
            ExiEvent::DocType(dt) => {
                lines.push(format!("DT(\"{}\")", &dt.text));
                i += 1;
            }
            ExiEvent::EntityReference(er) => {
                lines.push(format!("ER(\"{}\")", &er.name));
                i += 1;
            }
            ExiEvent::SelfContained => {
                lines.push("SC".to_string());
                i += 1;
            }
        }
    }

    lines.join("\n")
}

/// Normalisiert Attribut-Werte fuer kanonischen Vergleich.
///
/// - xsi:type: auf Local-Name reduziert ("{urn:foo}X" / "foo:X" -> "X")
/// - hexBinary-Werte: auf Uppercase normalisiert (EXI Spec 7.1.1)
fn normalize_at_value(qname: &erxi::qname::QName, value: &str) -> String {
    let is_xsi_type = &*qname.uri == "http://www.w3.org/2001/XMLSchema-instance"
        && &*qname.local_name == "type";

    if is_xsi_type {
        // Clark-Notation: "{uri}local" -> "local"
        if let Some(pos) = value.find('}') {
            return value[pos + 1..].to_string();
        }
        // Prefix-Form: "prefix:local" -> "local"
        if let Some(pos) = value.find(':') {
            return value[pos + 1..].to_string();
        }
        return value.to_string();
    }

    // hexBinary: nur Hex-Zeichen -> uppercase normalisieren
    if !value.is_empty() && value.chars().all(|c| c.is_ascii_hexdigit()) {
        return value.to_ascii_uppercase();
    }

    value.to_string()
}

fn format_qname_ex(q: &erxi::qname::QName, strip_uris: bool) -> String {
    if strip_uris || q.uri.is_empty() {
        q.local_name.to_string()
    } else {
        format!("{{{}}}{}", q.uri, q.local_name)
    }
}
