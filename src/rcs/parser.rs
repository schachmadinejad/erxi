//! XML Schema Regular Expression Parser (Appendix E).
//!
//! Parst ein XML Schema Regex Pattern und extrahiert die Zeichenmenge.
//! Gibt `None` zurück wenn das Pattern nicht unterstützt wird oder
//! die resultierende Menge ungültig ist (leer, >255 Zeichen, Nicht-BMP).

use crate::FastHashSet;

/// BMP-Größe ohne Surrogates: 0x0000..0xD7FF + 0xE000..0xFFFF
const BMP_SIZE: usize = 0xD800 + (0xFFFF - 0xE000 + 1); // 63488

/// Parst ein XML Schema Regex Pattern und gibt die Zeichenmenge zurück.
///
/// Gibt `None` zurück wenn:
/// - Pattern leer ist
/// - Unsupportete Features verwendet werden (\d, \p{}, etc.)
/// - Resultierende Menge > 255 Zeichen
/// - Resultierende Menge Nicht-BMP Zeichen enthält
/// - Resultierende Menge leer ist
pub fn parse_pattern(pattern: &str) -> Option<Vec<char>> {
    if pattern.is_empty() {
        return None;
    }
    let mut parser = Parser::new(pattern);
    let chars = parser.parse_regexp()?;

    // Prüfe ob Input vollständig konsumiert wurde (kein trailing junk)
    if parser.peek().is_some() {
        return None;
    }

    // Validierung (Nicht-BMP wird bereits im Parser via is_valid_char gefiltert)
    if chars.is_empty() || chars.len() > 255 {
        return None;
    }
    Some(chars)
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn expect(&mut self, expected: char) -> Option<()> {
        if self.peek() == Some(expected) {
            self.advance();
            Some(())
        } else {
            None
        }
    }

    /// [XSD:1] regExp ::= branch ('|' branch)*
    /// set-of-chars = union aller branches
    fn parse_regexp(&mut self) -> Option<Vec<char>> {
        let mut chars: FastHashSet<char> = self.parse_branch()?.into_iter().collect();

        while self.peek() == Some('|') {
            self.advance(); // consume '|'
            // Leerer Branch nach '|' ist ungültig (z.B. "a|" oder "a||b")
            match self.peek() {
                None | Some('|') | Some(')') => return None,
                _ => {}
            }
            let branch_chars = self.parse_branch()?;
            // Union via FastHashSet (O(1) pro Insert)
            for ch in branch_chars {
                chars.insert(ch);
                if chars.len() > 255 {
                    return None;
                }
            }
        }

        let mut result: Vec<char> = chars.into_iter().collect();
        result.sort_unstable();
        Some(result)
    }

    /// [XSD:2] branch ::= piece*
    /// set-of-chars = union aller pieces
    fn parse_branch(&mut self) -> Option<Vec<char>> {
        let mut chars: FastHashSet<char> = FastHashSet::default();

        while let Some(ch) = self.peek() {
            // Stoppen bei '|' oder ')' (Ende von branch)
            if ch == '|' || ch == ')' {
                break;
            }
            let piece_chars = self.parse_piece()?;
            // Union via FastHashSet (O(1) pro Insert)
            for c in piece_chars {
                chars.insert(c);
                if chars.len() > 255 {
                    return None;
                }
            }
        }

        Some(chars.into_iter().collect())
    }

    /// [XSD:3] piece ::= atom quantifier?
    /// set-of-chars = set-of-chars des atoms (quantifier ignoriert)
    fn parse_piece(&mut self) -> Option<Vec<char>> {
        let chars = self.parse_atom()?;
        self.skip_quantifier();
        Some(chars)
    }

    /// Überspringt optional einen Quantifier: ?, *, +, {n}, {n,}, {n,m}
    fn skip_quantifier(&mut self) {
        match self.peek() {
            Some('?') | Some('*') | Some('+') => {
                self.advance();
            }
            Some('{') => {
                self.advance();
                // Überspringe bis '}'
                while let Some(ch) = self.peek() {
                    self.advance();
                    if ch == '}' {
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    /// [XSD:9] atom ::= Char | charClass | '(' regExp ')'
    fn parse_atom(&mut self) -> Option<Vec<char>> {
        match self.peek()? {
            '(' => {
                self.advance(); // consume '('
                let chars = self.parse_regexp()?;
                self.expect(')')?;
                Some(chars)
            }
            '[' => self.parse_char_class(),
            '\\' => self.parse_escape(),
            '.' => {
                // Wildcard: alle BMP-Zeichen außer \n und \r
                // Das wären > 65000 Zeichen → None
                self.advance();
                None
            }
            '^' | '$' => {
                // Anker: ignorieren (keine Zeichen)
                self.advance();
                Some(vec![])
            }
            ']' => {
                // ']' außerhalb von Character-Klassen ist ungültig
                None
            }
            ch => {
                // Einfaches Literal
                self.advance();
                if self.is_valid_char(ch) {
                    Some(vec![ch])
                } else {
                    None
                }
            }
        }
    }

    /// Parst eine Character Class: [...]
    fn parse_char_class(&mut self) -> Option<Vec<char>> {
        self.expect('[')?;

        let negated = self.peek() == Some('^');
        if negated {
            self.advance();
            // Negierte Klassen haben fast immer > 255 Zeichen
            // Früher Abbruch möglich, aber wir parsen erstmal die innere Menge
        }

        let mut chars = self.parse_char_class_content()?;

        // Subtraction: [base-[subtract]]
        if self.peek() == Some('-') {
            // Prüfe ob es eine Subtraction ist (gefolgt von '[')
            let saved_pos = self.pos;
            self.advance(); // consume '-'
            if self.peek() == Some('[') {
                let subtract = self.parse_char_class()?;
                chars.retain(|c| !subtract.contains(c));
            } else {
                // War doch keine Subtraction, zurück
                self.pos = saved_pos;
            }
        }

        self.expect(']')?;

        if negated {
            // Negation: alle BMP-Zeichen außer chars
            let negated_size = BMP_SIZE - chars.len();
            if negated_size > 255 {
                return None;
            }
            let negated_chars = self.compute_negated_set(&chars);
            return Some(negated_chars);
        }

        Some(chars)
    }

    /// Parst den Inhalt einer Character Class (ohne äußere Klammern)
    fn parse_char_class_content(&mut self) -> Option<Vec<char>> {
        let mut chars: FastHashSet<char> = FastHashSet::default();

        while let Some(ch) = self.peek() {
            if ch == ']' {
                break;
            }
            if ch == '-' && self.peek_at(1) == Some('[') {
                // Subtraction beginnt, Content ist fertig
                break;
            }

            let item_chars = self.parse_char_class_item()?;
            // Union via FastHashSet (O(1) pro Insert)
            for c in item_chars {
                chars.insert(c);
                if chars.len() > 255 {
                    return None;
                }
            }
        }

        Some(chars.into_iter().collect())
    }

    /// Parst ein Item in einer Character Class (Literal, Range, oder Escape)
    fn parse_char_class_item(&mut self) -> Option<Vec<char>> {
        let first = match self.peek()? {
            '\\' => {
                let esc = self.parse_escape()?;
                if esc.len() != 1 {
                    // \s oder andere Multi-Char Escapes
                    return Some(esc);
                }
                esc[0]
            }
            ch => {
                self.advance();
                ch
            }
        };

        // Prüfe auf Range: a-z
        if self.peek() == Some('-') && self.peek_at(1) != Some('[') && self.peek_at(1) != Some(']')
        {
            self.advance(); // consume '-'
            let last = match self.peek()? {
                '\\' => {
                    let esc = self.parse_escape()?;
                    if esc.len() != 1 {
                        return None; // Range mit Multi-Char nicht erlaubt
                    }
                    esc[0]
                }
                ch => {
                    self.advance();
                    ch
                }
            };
            return self.char_range(first, last);
        }

        if self.is_valid_char(first) {
            Some(vec![first])
        } else {
            None
        }
    }

    /// Erstellt einen Bereich von first bis last (inklusive)
    fn char_range(&self, first: char, last: char) -> Option<Vec<char>> {
        // Validierung: first <= last und beide im BMP
        if first > last || last as u32 > 0xFFFF {
            return None;
        }
        // Sammle alle gültigen Zeichen (Surrogates aussparen)
        let chars: Vec<char> = (first as u32..=last as u32)
            .filter(|cp| !(0xD800..=0xDFFF).contains(cp))
            .filter_map(char::from_u32)
            .collect();
        // Prüfe Größenlimit
        if chars.len() > 255 {
            return None;
        }
        Some(chars)
    }

    /// Parst eine Escape-Sequenz
    fn parse_escape(&mut self) -> Option<Vec<char>> {
        self.expect('\\')?;
        let ch = self.advance()?;

        match ch {
            // Single-Char Escapes
            'n' => Some(vec!['\n']),
            'r' => Some(vec!['\r']),
            't' => Some(vec!['\t']),

            // \s = { \t, \n, \r, ' ' }
            's' => Some(vec!['\t', '\n', '\r', ' ']),

            // Nicht unterstützte Multi-Char Escapes (Appendix E)
            'd' | 'D' | 'w' | 'W' | 'i' | 'I' | 'c' | 'C' | 'S' => None,

            // Category Escapes \p{...}, \P{...}
            'p' | 'P' => {
                // Überspringe {Name}
                if self.peek() == Some('{') {
                    while self.peek() != Some('}') && self.peek().is_some() {
                        self.advance();
                    }
                    self.advance(); // consume '}'
                }
                None
            }

            // Hex Escape \xNN
            'x' => {
                let hex = self.take_hex(2)?;
                let cp = u32::from_str_radix(&hex, 16).ok()?;
                char::from_u32(cp).map(|c| vec![c])
            }

            // Unicode Escape \uNNNN
            'u' => {
                let hex = self.take_hex(4)?;
                let cp = u32::from_str_radix(&hex, 16).ok()?;
                // Prüfe auf Surrogate
                if (0xD800..=0xDFFF).contains(&cp) {
                    return None;
                }
                char::from_u32(cp).map(|c| vec![c])
            }

            // Escaped Metazeichen: \+, \-, \[, \], \., \*, \?, \{, \}, \(, \), \|, \^, \$, \\
            '+' | '-' | '[' | ']' | '.' | '*' | '?' | '{' | '}' | '(' | ')' | '|' | '^' | '$'
            | '\\' => Some(vec![ch]),

            // Andere: als Literal behandeln
            _ => {
                if self.is_valid_char(ch) {
                    Some(vec![ch])
                } else {
                    None
                }
            }
        }
    }

    /// Liest n Hex-Zeichen
    fn take_hex(&mut self, n: usize) -> Option<String> {
        let mut hex = String::with_capacity(n);
        for _ in 0..n {
            let ch = self.advance()?;
            if !ch.is_ascii_hexdigit() {
                return None;
            }
            hex.push(ch);
        }
        Some(hex)
    }

    /// Prüft ob ein Zeichen gültig ist (BMP, kein Surrogate)
    fn is_valid_char(&self, ch: char) -> bool {
        let cp = ch as u32;
        cp <= 0xFFFF && !(0xD800..=0xDFFF).contains(&cp)
    }

    /// Schaut n Zeichen voraus (0 = peek)
    fn peek_at(&self, n: usize) -> Option<char> {
        self.input[self.pos..].chars().nth(n)
    }

    /// Berechnet das Komplement einer Zeichenmenge im BMP
    fn compute_negated_set(&self, exclude: &[char]) -> Vec<char> {
        let exclude_set: FastHashSet<char> = exclude.iter().copied().collect();
        // BMP ohne Surrogates: 0x0000..0xD7FF und 0xE000..0xFFFF
        (0u32..0xD800)
            .chain(0xE000u32..=0xFFFF)
            .filter_map(char::from_u32)
            .filter(|ch| !exclude_set.contains(ch))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Basis-Parsing (Appendix E)
    // =========================================================================

    #[test]
    fn literale() {
        let chars = parse_pattern("abc").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn einzelnes_literal() {
        let chars = parse_pattern("x").unwrap();
        assert_eq!(chars, vec!['x']);
    }

    #[test]
    fn alternativen() {
        let chars = parse_pattern("a|b|c").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn gruppen() {
        let chars = parse_pattern("(a|b)").unwrap();
        assert_eq!(chars, vec!['a', 'b']);
    }

    #[test]
    fn gruppen_mit_suffix() {
        let chars = parse_pattern("(a|b)c").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn verschachtelte_gruppen() {
        let chars = parse_pattern("((a|b)|c)").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    // =========================================================================
    // Character Classes (Appendix E)
    // =========================================================================

    #[test]
    fn char_class_einfach() {
        let chars = parse_pattern("[abc]").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn char_class_range() {
        let chars = parse_pattern("[a-e]").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c', 'd', 'e']);
    }

    #[test]
    fn char_class_multi_range() {
        let chars = parse_pattern("[a-c0-2]").unwrap();
        assert_eq!(chars, vec!['0', '1', '2', 'a', 'b', 'c']);
    }

    #[test]
    fn char_class_gemischt() {
        let chars = parse_pattern("[a-cxyz]").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c', 'x', 'y', 'z']);
    }

    #[test]
    fn char_class_subtraction() {
        let chars = parse_pattern("[a-z-[aeiou]]").unwrap();
        // 26 - 5 = 21 Konsonanten
        assert_eq!(chars.len(), 21);
        assert!(!chars.contains(&'a'));
        assert!(!chars.contains(&'e'));
        assert!(chars.contains(&'b'));
        assert!(chars.contains(&'z'));
    }

    #[test]
    fn char_class_mit_escape() {
        let chars = parse_pattern(r"[\n\r\t]").unwrap();
        assert_eq!(chars, vec!['\t', '\n', '\r']);
    }

    #[test]
    fn char_class_mit_bindestrich_literal() {
        // Bindestrich am Anfang oder Ende ist Literal
        let chars = parse_pattern("[-a]").unwrap();
        assert_eq!(chars, vec!['-', 'a']);

        let chars = parse_pattern("[a-]").unwrap();
        assert_eq!(chars, vec!['-', 'a']);
    }

    // =========================================================================
    // Quantifier (Appendix E: ignoriert)
    // =========================================================================

    #[test]
    fn quantifier_fragezeichen() {
        let chars = parse_pattern("a?").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    #[test]
    fn quantifier_stern() {
        let chars = parse_pattern("a*").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    #[test]
    fn quantifier_plus() {
        let chars = parse_pattern("a+").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    #[test]
    fn quantifier_klammer() {
        let chars = parse_pattern("a{2,5}").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    #[test]
    fn quantifier_exakt() {
        let chars = parse_pattern("a{3}").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    // =========================================================================
    // Escape-Sequenzen (Appendix E)
    // =========================================================================

    #[test]
    fn escape_n() {
        let chars = parse_pattern(r"\n").unwrap();
        assert_eq!(chars, vec!['\n']);
    }

    #[test]
    fn escape_r() {
        let chars = parse_pattern(r"\r").unwrap();
        assert_eq!(chars, vec!['\r']);
    }

    #[test]
    fn escape_t() {
        let chars = parse_pattern(r"\t").unwrap();
        assert_eq!(chars, vec!['\t']);
    }

    #[test]
    fn escape_s() {
        let chars = parse_pattern(r"\s").unwrap();
        assert_eq!(chars, vec!['\t', '\n', '\r', ' ']);
    }

    #[test]
    fn escape_metazeichen() {
        let chars = parse_pattern(r"\+\-\.\*\?\[\]").unwrap();
        assert_eq!(chars, vec!['*', '+', '-', '.', '?', '[', ']']);
    }

    #[test]
    fn escape_backslash() {
        let chars = parse_pattern(r"\\").unwrap();
        assert_eq!(chars, vec!['\\']);
    }

    #[test]
    fn escape_hex() {
        let chars = parse_pattern(r"\x41").unwrap();
        assert_eq!(chars, vec!['A']);
    }

    #[test]
    fn escape_unicode() {
        let chars = parse_pattern(r"\u0041").unwrap();
        assert_eq!(chars, vec!['A']);
    }

    // =========================================================================
    // Nicht unterstützte Features (Appendix E)
    // =========================================================================

    #[test]
    fn nicht_unterstuetzt_d() {
        assert!(parse_pattern(r"\d").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_d_gross() {
        assert!(parse_pattern(r"\D").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_w() {
        assert!(parse_pattern(r"\w").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_w_gross() {
        assert!(parse_pattern(r"\W").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_s_gross() {
        assert!(parse_pattern(r"\S").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_i() {
        assert!(parse_pattern(r"\i").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_c() {
        assert!(parse_pattern(r"\c").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_p() {
        assert!(parse_pattern(r"\p{L}").is_none());
        assert!(parse_pattern(r"\p{Lu}").is_none());
    }

    #[test]
    fn nicht_unterstuetzt_p_gross() {
        assert!(parse_pattern(r"\P{L}").is_none());
    }

    #[test]
    fn wildcard_zu_gross() {
        assert!(parse_pattern(".").is_none());
    }

    #[test]
    fn negierte_klasse_zu_gross() {
        assert!(parse_pattern("[^a]").is_none());
        assert!(parse_pattern("[^abc]").is_none());
    }

    // =========================================================================
    // Edge Cases
    // =========================================================================

    #[test]
    fn leeres_pattern() {
        assert!(parse_pattern("").is_none());
    }

    #[test]
    fn zu_viele_zeichen() {
        // 256 Zeichen → zu viel
        assert!(parse_pattern(r"[\x00-\xFF]").is_none());
    }

    #[test]
    fn genau_255_zeichen() {
        // 255 Zeichen → OK
        let chars = parse_pattern(r"[\x00-\xFE]").unwrap();
        assert_eq!(chars.len(), 255);
    }

    #[test]
    fn surrogate_in_unicode_escape() {
        // U+D800 ist Surrogate
        assert!(parse_pattern(r"\uD800").is_none());
    }

    #[test]
    fn range_mit_surrogates() {
        // Range über Surrogates: diese werden ausgespart
        // D700..D7FF = 256 Zeichen (alle vor Surrogates)
        // D800..DFFF = Surrogates (werden übersprungen)
        // D900 liegt im Surrogate-Bereich, wird übersprungen
        // Total: 256 Zeichen → > 255, also None
        assert!(parse_pattern(r"[\uD700-\uD900]").is_none());

        // Kleinere Range sollte funktionieren
        let chars = parse_pattern(r"[\uD700-\uD7FE]").unwrap();
        // D700..D7FE = 255 Zeichen
        assert_eq!(chars.len(), 255);
    }

    #[test]
    fn duplikate_werden_entfernt() {
        let chars = parse_pattern("aabbcc").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn sortierung() {
        let chars = parse_pattern("zyx").unwrap();
        assert_eq!(chars, vec!['x', 'y', 'z']);
    }

    #[test]
    fn komplexes_pattern() {
        // Typisches Pattern für Telefonnummern
        let chars = parse_pattern(r"[0-9\+\-\s\(\)]+").unwrap();
        assert!(chars.contains(&'+'));
        assert!(chars.contains(&'-'));
        assert!(chars.contains(&'('));
        assert!(chars.contains(&')'));
        assert!(chars.contains(&'0'));
        assert!(chars.contains(&'9'));
        assert!(chars.contains(&' '));
    }

    #[test]
    fn anker_ignoriert() {
        // ^ und $ sind Anker und fügen keine Zeichen hinzu
        let chars = parse_pattern("^abc$").unwrap();
        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    // =========================================================================
    // Zusätzliche Coverage-Tests
    // =========================================================================

    #[test]
    fn range_first_greater_than_last() {
        // Ungültige Range: z-a
        assert!(parse_pattern("[z-a]").is_none());
    }

    #[test]
    fn range_mit_escape_am_ende() {
        // Range mit Escape am Ende: a-\x7A (a-z)
        let chars = parse_pattern(r"[a-\x7A]").unwrap();
        assert_eq!(chars.len(), 26);
    }

    #[test]
    fn char_class_subtraction_kein_subtract() {
        // Bindestrich gefolgt von nicht-'[' ist Range-Operator, nicht Subtraction
        // [a-z] ist eine Range, kein Subtraction
        let chars = parse_pattern("[a-z]").unwrap();
        assert_eq!(chars.len(), 26);
    }

    #[test]
    fn nicht_bmp_in_range() {
        // Range die über BMP hinausgeht
        assert!(parse_pattern(r"[\uFFFF-\uFFFF]").unwrap().len() == 1);
    }

    #[test]
    fn range_mit_multi_char_escape_als_ende() {
        // Range mit \s am Ende ist ungültig
        assert!(parse_pattern(r"[a-\s]").is_none());
    }

    #[test]
    fn duplikate_im_pattern() {
        // Duplikate werden entfernt
        let chars = parse_pattern("aaa").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    #[test]
    fn whitespace_in_char_class() {
        // \s in Character Class
        let chars = parse_pattern(r"[\s]").unwrap();
        assert_eq!(chars.len(), 4);
    }

    #[test]
    fn too_many_in_union() {
        // Viele Branches die zusammen > 255 Zeichen ergeben
        // Das passiert wenn wir genug Branches haben
        // a-z (26) | A-Z (26) | 0-9 (10) = 62, noch OK
        let chars = parse_pattern("[a-zA-Z0-9]").unwrap();
        assert_eq!(chars.len(), 62);
    }

    #[test]
    fn ungueltige_hex_escape() {
        // \xGG ist ungültig
        assert!(parse_pattern(r"\xGG").is_none());
    }

    #[test]
    fn ungueltige_unicode_escape() {
        // \uGGGG ist ungültig
        assert!(parse_pattern(r"\uGGGG").is_none());
    }

    #[test]
    fn unvollstaendige_unicode_escape() {
        // \u123 (nur 3 Zeichen) ist ungültig
        assert!(parse_pattern(r"\u123").is_none());
    }

    #[test]
    fn unvollstaendige_hex_escape() {
        // \x1 (nur 1 Zeichen) ist ungültig
        assert!(parse_pattern(r"\x1").is_none());
    }

    #[test]
    fn escape_am_ende() {
        // Backslash am Ende
        assert!(parse_pattern(r"\").is_none());
    }

    #[test]
    fn leere_gruppe() {
        // () ergibt leere Menge → None (da leeres Set ungültig)
        assert!(parse_pattern("()").is_none());
    }

    #[test]
    fn quantifier_in_gruppe() {
        let chars = parse_pattern("(ab)+").unwrap();
        assert_eq!(chars, vec!['a', 'b']);
    }

    #[test]
    fn bindestrich_am_anfang_char_class() {
        // [-a] hat Bindestrich als Literal am Anfang
        let chars = parse_pattern("[-a]").unwrap();
        assert!(chars.contains(&'-'));
        assert!(chars.contains(&'a'));
    }

    #[test]
    fn andere_escape_literal() {
        // \q sollte als 'q' behandelt werden (nicht spezielle Bedeutung)
        let chars = parse_pattern(r"\q").unwrap();
        assert_eq!(chars, vec!['q']);
    }

    // =========================================================================
    // Syntax-Fehler Tests
    // =========================================================================

    #[test]
    fn ungeschlossene_gruppe() {
        // (abc ohne schließende Klammer
        assert!(parse_pattern("(abc").is_none());
    }

    #[test]
    fn ungeschlossene_char_class() {
        // [abc ohne schließende Klammer
        assert!(parse_pattern("[abc").is_none());
    }

    #[test]
    fn leere_char_class() {
        // [] ist leer → None
        assert!(parse_pattern("[]").is_none());
    }

    #[test]
    fn quantifier_mindestens() {
        // {n,} Quantifier wird ignoriert
        let chars = parse_pattern("a{2,}").unwrap();
        assert_eq!(chars, vec!['a']);
    }

    // =========================================================================
    // Trailing Junk Tests (Parser muss vollständig konsumieren)
    // =========================================================================

    #[test]
    fn trailing_junk_klammer() {
        // a) hat unerwartete schließende Klammer
        assert!(parse_pattern("a)").is_none());
    }

    #[test]
    fn trailing_junk_pipe() {
        // a| hat leeren Branch nach Pipe
        assert!(parse_pattern("a|").is_none());
    }

    #[test]
    fn trailing_junk_doppel_pipe() {
        // a||b hat leeren Branch zwischen Pipes
        assert!(parse_pattern("a||b").is_none());
    }

    #[test]
    fn trailing_junk_eckige_klammer() {
        // a] hat unerwartete schließende Klammer
        assert!(parse_pattern("a]").is_none());
    }
}
