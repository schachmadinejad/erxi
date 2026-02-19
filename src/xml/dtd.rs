use crate::FastHashMap;

/// Extrahiert das DOCTYPE Internal Subset (Inhalt zwischen `[` und `]`).
///
/// Gibt `None` zurueck wenn kein `<!DOCTYPE ... [ ... ]>` gefunden wird.
fn extract_internal_subset(xml: &str) -> Option<&str> {
    let bracket_start = xml.find("<!DOCTYPE")?;
    let subset = &xml[bracket_start..];
    let open = subset.find('[')?;
    let close = subset[open..].find(']')?;
    Some(&subset[open + 1..open + close])
}

/// Extrahiert interne `<!ENTITY name "value">` Deklarationen aus dem DOCTYPE.
pub(crate) fn parse_dtd_entities(xml: &str) -> FastHashMap<String, String> {
    let mut entities = FastHashMap::default();

    let Some(internal_subset) = extract_internal_subset(xml) else {
        return entities;
    };

    // Parse <!ENTITY name "value"> oder <!ENTITY name 'value'>
    let mut pos = 0;
    let bytes = internal_subset.as_bytes();
    while pos < bytes.len() {
        if let Some(offset) = internal_subset[pos..].find("<!ENTITY") {
            let start = pos + offset + 8; // nach "<!ENTITY"
            // Whitespace überspringen
            let start = start + internal_subset[start..].find(|c: char| !c.is_whitespace()).unwrap_or(0);
            // Entity-Name lesen
            let name_end = start + internal_subset[start..].find(|c: char| c.is_whitespace()).unwrap_or(0);
            let name = &internal_subset[start..name_end];
            // Whitespace + Anführungszeichen
            let after_name = name_end + internal_subset[name_end..].find(|c: char| !c.is_whitespace()).unwrap_or(0);
            let quote = internal_subset.as_bytes().get(after_name).copied().unwrap_or(b'"');
            if quote == b'"' || quote == b'\'' {
                let value_start = after_name + 1;
                if let Some(value_end) = internal_subset[value_start..].find(quote as char) {
                    let value = &internal_subset[value_start..value_start + value_end];
                    entities.insert(name.to_string(), value.to_string());
                    pos = value_start + value_end + 1;
                    continue;
                }
            }
            pos = after_name + 1;
        } else {
            break;
        }
    }

    entities
}

/// Extrahiert externe Entity-Deklarationen (SYSTEM) aus dem DOCTYPE.
///
/// Gibt eine Map von Entity-Name → SYSTEM-URI zurueck.
pub(crate) fn parse_external_entities(xml: &str) -> FastHashMap<String, String> {
    let mut entities = FastHashMap::default();

    let Some(internal_subset) = extract_internal_subset(xml) else {
        return entities;
    };

    let mut pos = 0;
    while pos < internal_subset.len() {
        if let Some(offset) = internal_subset[pos..].find("<!ENTITY") {
            let start = pos + offset + 8;
            let start = start + internal_subset[start..].find(|c: char| !c.is_whitespace()).unwrap_or(0);
            let name_end = start + internal_subset[start..].find(|c: char| c.is_whitespace()).unwrap_or(0);
            let name = &internal_subset[start..name_end];
            let after_name = name_end + internal_subset[name_end..].find(|c: char| !c.is_whitespace()).unwrap_or(0);

            if internal_subset[after_name..].starts_with("SYSTEM") {
                let after_system = after_name + 6;
                let after_ws = after_system + internal_subset[after_system..].find(|c: char| !c.is_whitespace()).unwrap_or(0);
                let quote = internal_subset.as_bytes().get(after_ws).copied().unwrap_or(0);
                if quote == b'"' || quote == b'\'' {
                    let uri_start = after_ws + 1;
                    if let Some(uri_end) = internal_subset[uri_start..].find(quote as char) {
                        let uri = &internal_subset[uri_start..uri_start + uri_end];
                        entities.insert(name.to_string(), uri.to_string());
                        pos = uri_start + uri_end + 1;
                        continue;
                    }
                }
            }

            // Naechste Deklaration suchen
            if let Some(gt) = internal_subset[pos + offset..].find('>') {
                pos = pos + offset + gt + 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    entities
}
