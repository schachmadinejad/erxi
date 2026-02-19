use super::*;
    use crate::bitstream::{BitReader, BitWriter};
    use crate::event::{
        AtContent, ChContent, CmContent, DtContent, ErContent, NsContent, PiContent,
    };
    use crate::options::{Preserve, SchemaId};

    // ========================================================================
    // Schritt 1: Encoder Grundstruktur + Header
    // ========================================================================

    /// Encoder kann erstellt werden.
    #[test]
    fn encoder_new() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        assert!(!encoder.header_written);
    }

    /// Encoder mit ungültigen Options gibt Fehler zurück.
    ///
    /// Options-Validierung wird in prune() durchgeführt.
    #[test]
    fn encoder_new_invalid_options_error() {
        // strict + preserve.comments ist ungültig (Spec 5.4,
        // spec/exi-spec.txt lines 712-733).
        let options = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true,
                ..Default::default()
            },
            ..Default::default()
        };

        let result = Encoder::new(options, EncoderConfig::default());
        assert!(result.is_err());
    }

    /// Encoder mit Cookie-Config.
    #[test]
    fn encoder_config_with_cookie() {
        let config = EncoderConfig::with_cookie();
        assert!(config.include_cookie);
        assert!(!config.include_options);
    }

    /// Leerer Encoder gibt minimalen Header zurück.
    ///
    /// Spec 5.2, 5.3: Distinguishing Bits (10) + Version (0 für Version 1).
    /// Minimaler Header: 2 Bits (10) + 5 Bits (00000) = 7 Bits → 1 Byte (aufgerundet).
    #[test]
    fn encoder_finish_empty_minimal_header() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let bytes = encoder.finish().unwrap();

        // Minimaler Header ohne Cookie: 10 + 00000 = 0b1000_0000 = 0x80
        assert_eq!(bytes.len(), 1);
        assert_eq!(bytes[0], 0b1000_0000);
    }

    /// Encoder mit Cookie gibt "$EXI" + Header zurück.
    ///
    /// Spec 5.1: Cookie ist "$EXI" (4 Bytes).
    #[test]
    fn encoder_finish_with_cookie() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::with_cookie()).unwrap();
        let bytes = encoder.finish().unwrap();

        // Cookie: "$EXI" = 0x24, 0x45, 0x58, 0x49
        assert!(bytes.len() >= 5);
        assert_eq!(&bytes[0..4], b"$EXI");
        // Danach minimaler Header
        assert_eq!(bytes[4], 0b1000_0000);
    }

    /// finish() gibt Fehler bei offenen Elementen.
    #[test]
    fn encoder_finish_rejects_open_elements() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "a")))).unwrap();

        let err = encoder.finish().unwrap_err();
        assert!(err.to_string().contains("offene Elemente"), "{err}");
    }

    /// High-Level encode() Funktion.
    #[test]
    fn encode_empty_events() {
        let events: Vec<ExiEvent> = vec![];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();

        // Minimaler Header
        assert_eq!(bytes.len(), 1);
        assert_eq!(bytes[0], 0b1000_0000);
    }

    /// High-Level encode_with_config() mit Cookie.
    #[test]
    fn encode_with_config_cookie() {
        let events: Vec<ExiEvent> = vec![];
        let config = EncoderConfig::with_cookie();
        let bytes = encode_with_config(&events, &ExiOptions::default(), config).unwrap();

        assert!(bytes.starts_with(b"$EXI"));
    }

    /// encode() schreibt Options automatisch wenn non-default.
    #[test]
    fn encode_auto_includes_options_for_non_default() {
        let mut options = ExiOptions::default();
        options.set_alignment(Alignment::ByteAlignment);

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();

        // Decoder ohne explizite Options muss die Options aus dem Header lesen
        let (decoded, decoded_options) = crate::decoder::decode(&bytes).unwrap();
        assert_eq!(decoded, events);
        assert_eq!(decoded_options.alignment(), Alignment::ByteAlignment);
    }

    /// encode() schreibt keine Options bei Default-Optionen (Presence Bit = 0).
    #[test]
    fn encode_no_options_for_default() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();

        // Header: 2 Distinguishing Bits (10) + Presence Bit (0) = kein Options-Header
        // Erstes Byte: 1000_0000 (0x80) — Presence Bit ist Bit 2 (= 0)
        assert_eq!(bytes[0] & 0b0010_0000, 0, "Presence Bit muss 0 sein bei Default-Options");
    }

    /// Header wird nur einmal geschrieben.
    #[test]
    fn encoder_header_written_once() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        // Erstes Event triggert Header
        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        assert!(encoder.header_written);

        // Zweites Event schreibt keinen neuen Header
        let pos_before = encoder.writer.bit_position();
        encoder.write_header().unwrap();
        let pos_after = encoder.writer.bit_position();

        assert_eq!(pos_before, pos_after);
    }

    // ========================================================================
    // Schritt 2: Fidelity-Filter
    // ========================================================================

    /// Core Events werden immer encodiert.
    #[test]
    fn fidelity_core_events_always_encoded() {
        let options = ExiOptions::default();

        // SD, ED, SE, EE, AT, CH sind immer erlaubt
        assert!(should_encode_event(&ExiEvent::StartDocument, &options).unwrap());
        assert!(should_encode_event(&ExiEvent::EndDocument, &options).unwrap());
        assert!(
            should_encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "test"))), &options).unwrap()
        );
        assert!(should_encode_event(&ExiEvent::EndElement, &options).unwrap());
        assert!(
            should_encode_event(
                &ExiEvent::Attribute(AtContent {
                    qname: Rc::new(QName::new("", "attr")),
                    value: "val".into(),
                }),
                &options
            )
            .unwrap()
        );
        assert!(
            should_encode_event(
                &ExiEvent::Characters(ChContent {
                    value: "text".into(),
                }),
                &options
            )
            .unwrap()
        );
    }

    /// CM wird nur encodiert wenn preserve.comments=true.
    #[test]
    fn fidelity_comment_filtered() {
        let cm = ExiEvent::Comment(CmContent {
            text: "comment".into(),
        });

        // Default: comments=false
        let options_off = ExiOptions::default();
        assert!(!should_encode_event(&cm, &options_off).unwrap());

        // Mit comments=true
        let options_on = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(should_encode_event(&cm, &options_on).unwrap());
    }

    /// PI wird nur encodiert wenn preserve.pis=true.
    #[test]
    fn fidelity_pi_filtered() {
        let pi = ExiEvent::ProcessingInstruction(PiContent {
            name: "target".into(),
            text: "data".into(),
        });

        let options_off = ExiOptions::default();
        assert!(!should_encode_event(&pi, &options_off).unwrap());

        let options_on = ExiOptions {
            preserve: Preserve {
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(should_encode_event(&pi, &options_on).unwrap());
    }

    /// DT und ER werden nur encodiert wenn preserve.dtd=true.
    #[test]
    fn fidelity_dtd_filtered() {
        let dt = ExiEvent::DocType(DtContent {
            name: "root".into(),
            public: "".into(),
            system: "".into(),
            text: "".into(),
        });
        let er = ExiEvent::EntityReference(ErContent {
            name: "entity".into(),
        });

        let options_off = ExiOptions::default();
        assert!(!should_encode_event(&dt, &options_off).unwrap());
        assert!(!should_encode_event(&er, &options_off).unwrap());

        let options_on = ExiOptions {
            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(should_encode_event(&dt, &options_on).unwrap());
        assert!(should_encode_event(&er, &options_on).unwrap());
    }

    /// NS wird nur encodiert wenn preserve.prefixes=true.
    #[test]
    fn fidelity_ns_filtered() {
        let ns = ExiEvent::NamespaceDeclaration(NsContent {
            uri: "http://example.com".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        });

        let options_off = ExiOptions::default();
        assert!(!should_encode_event(&ns, &options_off).unwrap());

        let options_on = ExiOptions {
            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(should_encode_event(&ns, &options_on).unwrap());
    }

    /// SC wird vom Caller ignoriert (transparent gehandhabt).
    #[test]
    fn fidelity_sc_ignored() {
        let sc = ExiEvent::SelfContained;
        let options = ExiOptions::default();

        let result = should_encode_event(&sc, &options);
        assert_eq!(result.unwrap(), false);
    }

    /// Encoder überspringt gefilterte Events.
    #[test]
    fn encoder_skips_filtered_events() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        // CM wird übersprungen (preserve.comments=false)
        let cm = ExiEvent::Comment(CmContent {
            text: "comment".into(),
        });
        encoder.encode_event(&cm).unwrap();

        // Nur Header geschrieben, kein Event-Content
        let bytes = encoder.finish().unwrap();
        assert_eq!(bytes.len(), 1); // Nur minimaler Header
    }

    /// Encoder ignoriert explizite SC-Events (werden transparent gehandhabt).
    #[test]
    fn encoder_ignores_sc_event() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        // SC-Event wird übersprungen (should_encode_event returns false)
        let result = encoder.encode_event(&ExiEvent::SelfContained);
        assert!(result.is_ok());
    }

    // ========================================================================
    // Schritt 3: Event → Terminal Mapping
    // ========================================================================

    /// SD → Terminal::StartDocument
    #[test]
    fn event_to_terminal_sd() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&ExiEvent::StartDocument).unwrap();
        assert_eq!(terminal, Terminal::StartDocument);
    }

    /// ED → Terminal::EndDocument
    #[test]
    fn event_to_terminal_ed() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&ExiEvent::EndDocument).unwrap();
        assert_eq!(terminal, Terminal::EndDocument);
    }

    /// SE(qname) → Terminal::StartElement(QName)
    #[test]
    fn event_to_terminal_se() {
        let qname = QName::new("http://example.com", "element");
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder
            .event_to_terminal(&ExiEvent::StartElement(Rc::new(qname.clone())))
            .unwrap();
        let expected_id = encoder.interner.intern_expanded("http://example.com", "element").unwrap();
        assert_eq!(
            terminal,
            Terminal::StartElement(StartElementKind::QName(expected_id))
        );
    }

    /// EE → Terminal::EndElement
    #[test]
    fn event_to_terminal_ee() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&ExiEvent::EndElement).unwrap();
        assert_eq!(terminal, Terminal::EndElement);
    }

    /// AT(qname, val) → Terminal::Attribute(QName)
    #[test]
    fn event_to_terminal_at() {
        let qname = QName::new("", "attr");
        let at = ExiEvent::Attribute(AtContent {
            qname: Rc::new(qname.clone()),
            value: "value".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&at).unwrap();
        let expected_id = encoder.interner.intern_expanded("", "attr").unwrap();
        assert_eq!(
            terminal,
            Terminal::Attribute(AttributeKind::QName(expected_id))
        );
    }

    /// CH → Terminal::Characters
    #[test]
    fn event_to_terminal_ch() {
        let ch = ExiEvent::Characters(ChContent {
            value: "text".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&ch).unwrap();
        assert_eq!(terminal, Terminal::Characters);
    }

    /// NS → Terminal::NamespaceDecl
    #[test]
    fn event_to_terminal_ns() {
        let ns = ExiEvent::NamespaceDeclaration(NsContent {
            uri: "http://example.com".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&ns).unwrap();
        assert_eq!(terminal, Terminal::NamespaceDecl);
    }

    /// CM → Terminal::Comment
    #[test]
    fn event_to_terminal_cm() {
        let cm = ExiEvent::Comment(CmContent {
            text: "comment".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&cm).unwrap();
        assert_eq!(terminal, Terminal::Comment);
    }

    /// PI → Terminal::ProcessingInstr
    #[test]
    fn event_to_terminal_pi() {
        let pi = ExiEvent::ProcessingInstruction(PiContent {
            name: "target".into(),
            text: "data".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&pi).unwrap();
        assert_eq!(terminal, Terminal::ProcessingInstr);
    }

    /// DT → Terminal::DocType
    #[test]
    fn event_to_terminal_dt() {
        let dt = ExiEvent::DocType(DtContent {
            name: "root".into(),
            public: "".into(),
            system: "".into(),
            text: "".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&dt).unwrap();
        assert_eq!(terminal, Terminal::DocType);
    }

    /// ER → Terminal::EntityRef
    #[test]
    fn event_to_terminal_er() {
        let er = ExiEvent::EntityReference(ErContent {
            name: "entity".into(),
        });
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let terminal = encoder.event_to_terminal(&er).unwrap();
        assert_eq!(terminal, Terminal::EntityRef);
    }

    /// SC → Terminal::SelfContained
    #[test]
    fn event_to_terminal_sc() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let result = encoder.event_to_terminal(&ExiEvent::SelfContained);
        assert_eq!(result.unwrap(), Terminal::SelfContained);
    }

    // ========================================================================
    // Schritt 4: Production-Lookup
    // ========================================================================

    /// Exakter Terminal-Match in Document Grammar.
    #[test]
    fn find_production_exact_match() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let grammar = GrammarSystem::built_in_document();

        // SD ist in Document Grammar mit exaktem Match
        let terminal = Terminal::StartDocument;
        let prod = encoder
            .find_production(&grammar, grammar.start(), &terminal)
            .unwrap();
        assert_eq!(prod.terminal, terminal);
    }

    /// Wildcard-Fallback für SE(*) in Document Grammar.
    #[test]
    fn find_production_wildcard_se() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let mut grammar = GrammarSystem::built_in_document();
        grammar.prune(&ExiOptions::default()).unwrap();

        // DocContent enthält SE(*), nicht SE(qname)
        let mut interner = crate::qname::StringInterner::new();
        let test_id = interner.intern_expanded("", "test").unwrap();
        let terminal =
            Terminal::StartElement(StartElementKind::QName(test_id));
        let prod = encoder
            .find_production(&grammar, NonTerminalId::DocContent, &terminal)
            .unwrap();

        // Sollte SE(*) matchen
        assert_eq!(
            prod.terminal,
            Terminal::StartElement(StartElementKind::Wildcard)
        );
    }

    /// Kein Match → Error::InvalidEventCode.
    #[test]
    fn find_production_no_match() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let mut grammar = GrammarSystem::built_in_document();
        grammar.prune(&ExiOptions::default()).unwrap();

        // CM ist nicht in geprunter Document Grammar (preserve.comments=false)
        let terminal = Terminal::Comment;
        let result = encoder.find_production(&grammar, NonTerminalId::DocContent, &terminal);

        assert!(matches!(result, Err(Error::InvalidEventCode { .. })));
    }

    /// Unbekanntes NonTerminal → Error::UnknownNonTerminal.
    #[test]
    fn find_production_unknown_nt() {
        let encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();
        let grammar = GrammarSystem::built_in_document();

        // ElementContent existiert nicht in Document Grammar
        let terminal = Terminal::EndElement;
        let result = encoder.find_production(&grammar, NonTerminalId::ElementContent, &terminal);

        assert!(matches!(
            result,
            Err(Error::UnknownNonTerminal(_))
        ));
    }

    // ========================================================================
    // Schritt 7: String Table für QNames
    // ========================================================================

    /// Spec 7.3.2: URI Miss encodiert 0 + String Literal.
    #[test]
    fn string_table_uri_miss() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // Nach dem Encoding sollte die URI in der String Table sein
        assert!(
            encoder
                .string_table
                .lookup_uri("http://example.org")
                .is_some()
        );
    }

    /// Spec 7.3.2: URI Hit encodiert i+1 (Compact ID).
    #[test]
    fn string_table_uri_hit() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "child"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // URI sollte nur einmal in der String Table sein (Hit beim zweiten Mal)
        // Appendix D.1: 3 pre-populated URIs (IDs 0, 1, 2), neue URI bekommt ID 3
        assert_eq!(
            encoder.string_table.lookup_uri("http://example.org"),
            Some(3)
        );
    }

    /// Spec 7.3.3: LocalName Miss encodiert length + String.
    #[test]
    fn string_table_local_name_miss() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // LocalName sollte in der String Table sein
        let uri_id = encoder.string_table.lookup_uri("").unwrap();
        assert!(
            encoder
                .string_table
                .lookup_local_name(uri_id, "root")
                .is_some()
        );
    }

    /// Spec 7.3.3: LocalName Hit encodiert Compact ID.
    #[test]
    fn string_table_local_name_hit() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        // Verschachtelte Elemente mit gleichem LocalName
        // Das zweite "item" sollte ein Hit in der String Table sein
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))), // Hit
            ExiEvent::EndElement,
            ExiEvent::EndElement, // </root>
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // LocalName "item" sollte nur einmal in der String Table sein
        // (root und item sind verschiedene LocalNames)
        let uri_id = encoder.string_table.lookup_uri("").unwrap();
        // root + item = 2 verschiedene LocalNames
        assert_eq!(encoder.string_table.local_name_count(uri_id), 2);
        // "item" sollte vorhanden sein
        assert!(
            encoder
                .string_table
                .lookup_local_name(uri_id, "item")
                .is_some()
        );
    }

    /// Spec 7.3: Wiederholte Elemente erzeugen kleinere Encoding-Größe (Hit statt Miss).
    #[test]
    fn string_table_repeated_element_smaller() {
        let qname = Rc::new(QName::new("http://example.org", "element"));

        // Erstes Dokument: Element wird einmal verwendet
        let events_once = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(qname.clone()),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes_once = encode(&events_once, &ExiOptions::default()).unwrap();

        // Zweites Dokument: Element wird zweimal verwendet
        let events_twice = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(qname.clone()),
            ExiEvent::StartElement(qname.clone()), // Nested, zweites Mal
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes_twice = encode(&events_twice, &ExiOptions::default()).unwrap();

        // Das zweite Element (Hit) sollte weniger Bytes brauchen als das erste (Miss)
        // Daher: 2 Elemente sollten weniger als 2× Größe von 1 Element sein
        assert!(bytes_twice.len() < bytes_once.len() * 2);
    }

    /// Spec 7.3.2, Appendix D.1: Pre-populated URIs (leere URI, XML, XSI) sind immer in der Table.
    #[test]
    fn string_table_pre_populated_uris() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        // Leere URI sollte Hit sein (pre-populated)
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // Leere URI hat ID 0 (pre-populated)
        assert_eq!(encoder.string_table.lookup_uri(""), Some(0));
        // Appendix D.1: uri_count ist immer 3 (keine neue URI hinzugefügt)
        assert_eq!(encoder.string_table.uri_count(), 3);
    }

    // ========================================================================
    // Schritt 8: AT Encoding + Sortierung
    // ========================================================================

    /// Spec 6: Attribute werden mit QName + Value encodiert.
    #[test]
    fn at_encoding_single_attribute() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "id")),
                value: "123".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Spec 6: Mehrere Attribute werden encodiert.
    #[test]
    fn at_encoding_multiple_attributes() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "a")),
                value: "1".into(),
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "b")),
                value: "2".into(),
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "c")),
                value: "3".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Spec 7.3: AT Values werden in der String Table gespeichert.
    #[test]
    fn at_value_in_string_table() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let qname = Rc::new(QName::new("", "attr"));
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: qname.clone(),
                value: "testvalue".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // Value sollte in der String Table sein (wenn non-empty)
        // Note: encode_value fügt den Wert zur String Table hinzu
        // Aber wir nutzen aktuell string::encode direkt
    }

    // ========================================================================
    // Schritt 9: CH Encoding
    // ========================================================================

    /// Spec 7.1.10: CH Content ist ein String-Wert.
    #[test]
    fn ch_encoding_text_content() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "Hello, World!".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Spec 7.1.10: CH mit Unicode-Zeichen.
    #[test]
    fn ch_encoding_unicode() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "日本語 🎉 Grüße".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Spec 7.1.10: CH mit leerem String.
    #[test]
    fn ch_encoding_empty_string() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    // ========================================================================
    // Schritt 10: Grammar Evolution
    // ========================================================================

    /// Spec 8.4.3: Element Grammar evolviert bei SE(*) Match.
    ///
    /// Nach SE(*) Match wird SE(qname) zur Grammar hinzugefügt.
    /// Beim zweiten Auftreten des gleichen Elements wird SE(qname) mit kleinerem
    /// Event Code verwendet.
    #[test]
    fn grammar_evolution_se_wildcard() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            // Erstes "child" → SE(*) Match → lernt SE(child)
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::EndElement,
            // Zweites "child" → SE(child) Match (gelernt)
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // Die Element Grammar für "root" sollte SE(child) gelernt haben
        let root_expanded = encoder.interner.intern_expanded("", "root").unwrap();
        let root_grammar = encoder.element_grammars.get(&GrammarKey {
            qname: root_expanded,
            local_id: None,
        });
        assert!(root_grammar.is_some());

        // Prüfen ob SE(child) in der Grammar ist
        if let Some(grammar) = root_grammar {
            let element_content = grammar.get(NonTerminalId::ElementContent);
            assert!(element_content.is_some());
            if let Some(nt) = element_content {
                let child_id = encoder.interner.intern_expanded("", "child").unwrap();
                let child_terminal =
                    Terminal::StartElement(StartElementKind::QName(child_id));
                assert!(nt.has_terminal(&child_terminal));
            }
        }
    }

    /// Spec 8.4.3: Element Grammar evolviert bei AT(*) Match.
    #[test]
    fn grammar_evolution_at_wildcard() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            // Erstes "id" Attribut → AT(*) Match → lernt AT(id)
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "id")),
                value: "1".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        for event in &events {
            encoder.encode_event(event).unwrap();
        }

        // Die Element Grammar für "root" sollte AT(id) gelernt haben
        let root_expanded = encoder.interner.intern_expanded("", "root").unwrap();
        let root_grammar = encoder.element_grammars.get(&GrammarKey {
            qname: root_expanded,
            local_id: None,
        });
        assert!(root_grammar.is_some());

        if let Some(grammar) = root_grammar {
            let start_tag = grammar.get(NonTerminalId::StartTagContent);
            assert!(start_tag.is_some());
            if let Some(nt) = start_tag {
                let id_id = encoder.interner.intern_expanded("", "id").unwrap();
                let id_terminal =
                    Terminal::Attribute(AttributeKind::QName(id_id));
                assert!(nt.has_terminal(&id_terminal));
            }
        }
    }

    // ========================================================================
    // Schritt 11: NS/CM/PI/DT/ER Encoding
    // ========================================================================

    /// Spec 4: NS Event wird encodiert wenn preserve.prefixes=true.
    #[test]
    fn ns_encoding_with_preserve_prefixes() {
        let options = ExiOptions {
            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "http://example.org".into(),
                prefix: "ex".into(),
                local_element_ns: true,
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 4: CM Event wird encodiert wenn preserve.comments=true.
    #[test]
    fn cm_encoding_with_preserve_comments() {
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Comment(CmContent {
                text: "This is a comment".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 4: PI Event wird encodiert wenn preserve.pis=true.
    #[test]
    fn pi_encoding_with_preserve_pis() {
        let options = ExiOptions {
            preserve: Preserve {
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::ProcessingInstruction(PiContent {
                name: "xml-stylesheet".into(),
                text: "type=\"text/xsl\"".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 4: DT Event wird encodiert wenn preserve.dtd=true.
    #[test]
    fn dt_encoding_with_preserve_dtd() {
        let options = ExiOptions {
            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::DocType(DtContent {
                name: "html".into(),
                public: "-//W3C//DTD XHTML 1.0//EN".into(),
                system: "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd".into(),
                text: "".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("", "html"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 4: ER Event wird encodiert wenn preserve.dtd=true.
    #[test]
    fn er_encoding_with_preserve_dtd() {
        let options = ExiOptions {
            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EntityReference(ErContent {
                name: "nbsp".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    // ========================================================================
    // Schritt 12: Alignment-Modi
    // ========================================================================

    /// Spec 7: bit-packed Alignment (Default).
    #[test]
    fn alignment_bit_packed() {
        let options = ExiOptions::default(); // Default ist BitPacked
        assert_eq!(options.alignment, Alignment::BitPacked);

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();
        // bit-packed produziert kompaktere Ausgabe
        assert!(!bytes.is_empty());
    }

    /// Spec 7: byte-aligned Alignment.
    #[test]
    fn alignment_byte_aligned() {
        use crate::options::Alignment;

        let options = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();
        assert!(!bytes.is_empty());
    }

    /// Spec 7: pre-compression Alignment.
    #[test]
    fn alignment_pre_compression() {
        use crate::options::Alignment;

        let options = ExiOptions {
            alignment: Alignment::PreCompression,

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();
        assert!(!bytes.is_empty());
    }

    /// Spec 7: byte-aligned produziert größere Ausgabe als bit-packed.
    #[test]
    fn alignment_byte_aligned_larger_than_bit_packed() {
        use crate::options::Alignment;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "element"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "attr")),
                value: "value".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "text content".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bit_packed = encode(&events, &ExiOptions::default()).unwrap();

        let byte_aligned_opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        let byte_aligned = encode(&events, &byte_aligned_opts).unwrap();

        // byte-aligned sollte >= bit-packed sein (normalerweise größer)
        assert!(byte_aligned.len() >= bit_packed.len());
    }

    /// SchemaId::Id ohne Schema ist ungueltig.
    #[test]
    fn schema_id_requires_schema() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::EndDocument,
        ];
        let mut options = ExiOptions::default();
        options.set_schema_id(Some(SchemaId::Id("http://example.org/schema".into())));
        let result = encode(&events, &options);
        assert!(matches!(result, Err(Error::InvalidOptionCombination)));
    }

    /// SchemaId::None/BuiltinOnly mit Schema ist ungueltig.
    #[test]
    fn schema_id_forbids_schema() {
        use crate::schema::{SchemaInfo, ElementDeclaration};
        let qname = Rc::new(QName::new("", "root"));
        let decl = Rc::new(ElementDeclaration::new(qname.clone()));
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .all_element(QName::new("", "root"))
            .element_declaration(qname.clone(), decl)
            .build();

        let mut options = ExiOptions::default();
        options.set_schema_id(Some(SchemaId::None));
        let result = encode_with_schema(&[], &options, &schema);
        assert!(matches!(result, Err(Error::InvalidOptionCombination)));

        options.set_schema_id(Some(SchemaId::BuiltinOnly));
        let result = encode_with_schema(&[], &options, &schema);
        assert!(matches!(result, Err(Error::InvalidOptionCombination)));
    }

    /// DTRM mit User-defined Representation wird ueber Registry aufgeloest.
    #[test]
    fn dtrm_user_defined_roundtrip() {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};
        use crate::typed_value::{
            register_user_defined_representation, unregister_user_defined_representation, XSD_NS,
        };

        fn encode_ud(writer: &mut BitWriter, value: &str, _alignment: Alignment) -> Result<()> {
            crate::string::encode(writer, value);
            Ok(())
        }
        fn decode_ud(reader: &mut BitReader, _alignment: Alignment) -> Result<String> {
            crate::string::decode(reader)
        }

        let type_qname = Rc::new(QName::new("http://example.org", "MyString"));
        let type_def = Rc::new(TypeDefinition::Simple {
            name: Some(type_qname.clone()),
            variety: crate::schema::SimpleTypeVariety::Atomic,
            base_type_qname: Some(Rc::new(QName::new(XSD_NS, "string"))),
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        });

        let elem_qname = Rc::new(QName::new("", "root"));
        let decl = Rc::new(ElementDeclaration::new(elem_qname.clone()).with_type(type_def.clone()));
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .all_element(QName::new("", "root"))
            .element_declaration(elem_qname.clone(), decl)
            .type_definition(type_qname.clone(), type_def)
            .build();

        let repr_qname = QName::new("http://example.org/codec", "myCodec");
        register_user_defined_representation(repr_qname.clone(), encode_ud, decode_ud);

        let mut options = ExiOptions::default();
        options.set_datatype_representation_map(vec![
            crate::options::DatatypeRepresentationMapping {
                type_qname: (*type_qname).clone(),
                representation_qname: repr_qname.clone(),
            },
        ]);

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(elem_qname.clone()),
            ExiEvent::Characters(ChContent { value: "hello".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&bytes, options, &schema).unwrap();
        assert_eq!(decoded, events);

        unregister_user_defined_representation(&repr_qname);
    }

    // ========================================================================
    // Schritt 13 + 14: High-Level API + Edge Cases
    // ========================================================================

    /// Spec 6: Komplettes Dokument mit allen Event-Typen.
    #[test]
    fn encode_complete_document() {
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::ProcessingInstruction(PiContent {
                name: "xml".into(),
                text: "version=\"1.0\"".into(),
            }),
            ExiEvent::Comment(CmContent {
                text: "Header comment".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "version")),
                value: "1.0".into(),
            }),
            ExiEvent::Comment(CmContent {
                text: "Inside element".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "child"))),
            ExiEvent::Characters(ChContent {
                value: "Text content".into(),
            }),
            ExiEvent::EndElement, // </child>
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "child"))),
            ExiEvent::Characters(ChContent {
                value: "More text".into(),
            }),
            ExiEvent::EndElement, // </child>
            ExiEvent::EndElement, // </root>
            ExiEvent::Comment(CmContent {
                text: "Trailing comment".into(),
            }),
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());
        let bytes = result.unwrap();
        assert!(bytes.len() > 10); // Sollte substantielle Ausgabe haben
    }

    /// Edge Case: EE ohne SE ist ein Fehler.
    #[test]
    fn edge_case_ee_without_se() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement, // Extra EE ohne SE
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &ExiOptions::default());
        // Sollte fehlschlagen weil Element-Stack leer ist
        assert!(result.is_err());
    }

    /// Edge Case: Explizites SC Event wird ignoriert (transparente Behandlung).
    #[test]
    fn edge_case_sc_ignored() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::SelfContained,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // SC-Event wird übersprungen, Rest wird normal encodiert
        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Edge Case: Tief verschachtelte Elemente.
    #[test]
    fn edge_case_deep_nesting() {
        let mut events = vec![ExiEvent::StartDocument];

        for i in 0..50 {
            events.push(ExiEvent::StartElement(Rc::new(QName::new(
                "",
                format!("level{}", i),
            ))));
        }
        for _ in 0..50 {
            events.push(ExiEvent::EndElement);
        }
        events.push(ExiEvent::EndDocument);

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    /// Edge Case: Viele Attribute an einem Element.
    #[test]
    fn edge_case_many_attributes() {
        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        for i in 0..20 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{}", i))),
                value: format!("value{}", i).into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    // ========================================================================
    // Schritt 5: Minimaler Document (SD + ED)
    // ========================================================================

    /// Leeres Document: SD + SE + EE + ED.
    ///
    /// Ein minimales valides EXI Document braucht mindestens ein Element.
    /// SD → DocContent, SE → DocEnd, EE → ..., ED
    #[test]
    fn encode_minimal_document() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();

        // Header (1 Byte: 0x80) + Event Codes
        // SD: [0] in Document (1 Bit = 0)
        // SE(*): [0] in DocContent (1 Bit = 0) + QName Content
        // EE: in Element Grammar
        // ED: [0] in DocEnd (1 Bit = 0)
        assert!(!bytes.is_empty());
        // Der erste Byte ist der Header
        assert_eq!(bytes[0], 0b1000_0000);
    }

    /// SD Event Code ist [0] in Document Grammar.
    #[test]
    fn encode_sd_event_code() {
        let mut encoder = Encoder::new(ExiOptions::default(), EncoderConfig::default()).unwrap();

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();

        // Nach SD sollte document_nt zu DocContent gewechselt sein
        assert_eq!(encoder.document_nt, NonTerminalId::DocContent);
    }

    /// ED Event Code wird korrekt encodiert.
    ///
    /// Nach SE → DocEnd, dann ED mit Event Code [0].
    #[test]
    fn encode_ed_event_code() {
        // Für diesen Test brauchen wir einen vollständigen Ablauf
        // SD → DocContent → (SE) → DocEnd → (ED)
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Sollte ohne Fehler durchlaufen
        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    // ========================================================================
    // EXI Compression Tests (Spec 9)
    // ========================================================================

    /// Spec 9: compression=true aktiviert DEFLATE.
    #[test]
    fn compression_basic() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "attr")),
                value: "value".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "text content".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode(&events, &options);
        assert!(result.is_ok());

        // Bei Compression sollte der Output komprimiert sein
        let bytes = result.unwrap();
        assert!(!bytes.is_empty());
    }

    /// Spec 9.3: ≤100 Values in einem Compressed Stream.
    #[test]
    fn compression_small_block() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // 50 Attribute (≤100 Values)
        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 9.3: >100 Values in mehreren Compressed Streams.
    #[test]
    fn compression_large_block() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // 150 Attribute (>100 Values)
        for i in 0..150 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Spec 9: compression=true mit mehreren gleichen QNames.
    #[test]
    fn compression_same_qname_channel() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // Mehrere Elemente mit gleichen Attributen → ein Value Channel
        for i in 0..5 {
            events.push(ExiEvent::StartElement(Rc::new(QName::new("", "item"))));
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "id")),
                value: format!("{i}").into(),
            }));
            events.push(ExiEvent::EndElement);
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    // ========================================================================
    // Schema-informed Encoder Tests (Issue #37)
    // ========================================================================

    /// Encoder::with_schema() erstellt Schema-informed Encoder.
    #[test]
    fn encoder_with_schema_creates_encoder() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let encoder = Encoder::with_schema(
            ExiOptions::default(),
            EncoderConfig::default(),
            schema,
        );
        assert!(encoder.is_ok());
    }

    /// Schema-informed Encoder verwendet Schema-informed Document Grammar.
    #[test]
    fn encoder_with_schema_uses_schema_grammar() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .global_element(QName::new("http://example.org", "author"))
            .build();

        let encoder = Encoder::with_schema(
            ExiOptions::default(),
            EncoderConfig::default(),
            schema,
        ).unwrap();

        // Document Grammar sollte Schema-informed sein
        // Das bedeutet SE(book) und SE(author) sind explizite Productions
        assert!(encoder.schema.is_some());
    }

    /// encode_with_schema() High-Level API funktioniert.
    #[test]
    fn encode_with_schema_basic() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode_with_schema(&events, &ExiOptions::default(), &schema);
        assert!(result.is_ok());
    }

    /// Schema-informed Encoding mit pre-populated String Table.
    ///
    /// Spec 7.3.1: Schema LocalNames werden pre-populated.
    #[test]
    fn encoder_with_schema_prepopulates_string_table() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .all_element(QName::new("http://example.org", "chapter"))
            .attribute(QName::new("", "id"))
            .build();

        let encoder = Encoder::with_schema(
            ExiOptions::default(),
            EncoderConfig::default(),
            schema,
        ).unwrap();

        // Schema-URIs sollten pre-populated sein
        assert!(encoder.string_table.lookup_uri("http://example.org").is_some());

        // LocalNames sollten pre-populated sein
        let uri_id = encoder.string_table.lookup_uri("http://example.org").unwrap();
        assert!(encoder.string_table.lookup_local_name(uri_id, "book").is_some());
        assert!(encoder.string_table.lookup_local_name(uri_id, "chapter").is_some());
    }

    /// Schema-informed vs Schema-less Encoding kann unterschiedliche Größe haben.
    ///
    /// Schema-informed mit pre-populated String Table sollte kompakter sein,
    /// weil QNames von Anfang an als Hits encodiert werden können.
    #[test]
    fn schema_informed_vs_schemaless_encoding() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "book"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Schema-less Encoding
        let bytes_schemaless = encode(&events, &ExiOptions::default()).unwrap();

        // Schema-informed Encoding
        let bytes_schema = encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();

        // Beide sollten erfolgreich sein
        // Schema-informed kann kleiner sein wegen pre-populated String Table
        // (aber nicht garantiert bei kleinen Dokumenten)
        assert!(!bytes_schemaless.is_empty());
        assert!(!bytes_schema.is_empty());
    }

    /// Spec 7.1: Typed Value Encoding mit Integer.
    ///
    /// Bei Schema-informed Encoding werden Werte mit ihrem Schema-Typ encodiert.
    /// Integer-Werte werden als EXI Integer (nicht String) encodiert.
    #[test]
    fn typed_value_encoding_integer() {
        use crate::schema::{SchemaInfo, TypeDefinition, ElementDeclaration};

        // Schema mit Integer-Element
        let int_type = Rc::new(TypeDefinition::simple_with_base("integer"));
        let elem_qname = Rc::new(QName::new("", "count"));
        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_type(int_type),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "count"))
            .element_declaration(elem_qname, root_elem)
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "count"))),
            ExiEvent::Characters(ChContent { value: "42".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Schema-informed Encoding sollte Integer-Encoding verwenden
        let bytes = encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();

        // Encoding sollte erfolgreich sein
        assert!(!bytes.is_empty());

        // Schema-less Encoding zum Vergleich (sollte größer sein wegen String-Encoding)
        let bytes_schemaless = encode(&events, &ExiOptions::default()).unwrap();

        // Schema-informed mit Integer-Typ sollte kompakter sein
        // (Integer 42 = wenige Bits vs String "42" = Länge + 2 Unicode Code Points)
        // Aber: Schema-informed hat auch Schema-String-Table, also ist Größenvergleich nicht trivial
        assert!(!bytes_schemaless.is_empty());
    }

    /// Spec 7.1: Typed Value Encoding mit Boolean.
    #[test]
    fn typed_value_encoding_boolean() {
        use crate::schema::{SchemaInfo, TypeDefinition, ElementDeclaration};

        let bool_type = Rc::new(TypeDefinition::simple_with_base("boolean"));
        let elem_qname = Rc::new(QName::new("", "flag"));
        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_type(bool_type),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "flag"))
            .element_declaration(elem_qname, root_elem)
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "flag"))),
            ExiEvent::Characters(ChContent { value: "true".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();
        assert!(!bytes.is_empty());
    }

    /// Spec 8.5.4.4.2: xsi:nil="true" erlaubt nur EE.
    #[test]
    fn xsi_nil_allows_only_ee() {
        use crate::schema::{SchemaInfo, ElementDeclaration};

        let elem_qname = Rc::new(QName::new("", "item"));
        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_nillable(true),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "item"))
            .element_declaration(elem_qname, root_elem)
            .build();

        // xsi:nil="true" gefolgt von EE ist OK
        let events_ok = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_nil()),
                value: "true".into(),
            }),
            ExiEvent::EndElement,  // EE ist erlaubt
            ExiEvent::EndDocument,
        ];

        let result = encode_with_schema(&events_ok, &ExiOptions::default(), &schema);
        assert!(result.is_ok());

        // Spec 8.5.4.4.2: Bei strict=true ist CH nach xsi:nil="true" NICHT erlaubt
        let events_error = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_nil()),
                value: "true".into(),
            }),
            ExiEvent::Characters(ChContent { value: "content".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut strict_opts = ExiOptions::default();
        strict_opts.strict = true;
        let result = encode_with_schema(&events_error, &strict_opts, &schema);
        assert!(matches!(result, Err(Error::XsiNilContentNotEmpty)));
    }

    /// Spec 8.5.4.4: xsi:type wechselt den Element-Typ.
    #[test]
    fn xsi_type_changes_element_type() {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};

        // Schema mit einem Typ erstellen
        let type_qname = Rc::new(QName::new("http://www.w3.org/2001/XMLSchema", "string"));
        let string_type = Rc::new(TypeDefinition::simple_with_base("string"));

        let elem_qname = Rc::new(QName::new("", "item"));
        let root_elem = Rc::new(ElementDeclaration::new(elem_qname.clone()));

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "item"))
            .element_declaration(elem_qname, root_elem)
            .type_definition(type_qname, string_type)
            .build();

        // Events mit xsi:type
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "xs:string".into(),
            }),
            ExiEvent::Characters(ChContent { value: "test".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode_with_schema(&events, &ExiOptions::default(), &schema);
        assert!(result.is_ok());
    }

    /// Spec 8.5.4.4: xsi:type mit unbekanntem Typ-Wert.
    ///
    /// Wenn der xsi:type-Wert nicht auf einen bekannten Typ im Schema zeigt,
    /// wird der Grammar-Switch übersprungen und das Encoding fortgesetzt.
    /// Das gilt auch bei strict=true (Exificient-kompatibles Verhalten).
    #[test]
    fn xsi_type_not_found_tolerant() {
        use crate::schema::{SchemaInfo, ElementDeclaration};

        let elem_qname = Rc::new(QName::new("", "item"));
        let root_elem = Rc::new(ElementDeclaration::new(elem_qname.clone()));

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "item"))
            .element_declaration(elem_qname, root_elem)
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "unknownType".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        // Encoding gelingt — unbekannter xsi:type-Wert wird als String encodiert,
        // Grammar-Switch wird übersprungen.
        let result = encode_with_schema(&events, &options, &schema);
        assert!(result.is_ok(), "Expected Ok, got {:?}", result);
    }

    /// Spec 8.5.4.4.2: xsi:type und xsi:nil können nicht zusammen verwendet werden.
    #[test]
    fn xsi_type_and_nil_together_error() {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};

        let type_qname = Rc::new(QName::new("http://www.w3.org/2001/XMLSchema", "string"));
        let string_type = Rc::new(TypeDefinition::simple_with_base("string"));

        let elem_qname = Rc::new(QName::new("", "item"));
        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_nillable(true),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "item"))
            .element_declaration(elem_qname, root_elem)
            .type_definition(type_qname, string_type)
            .build();

        // xsi:type gefolgt von xsi:nil - NICHT erlaubt
        let events_type_then_nil = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "xs:string".into(),
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_nil()),
                value: "true".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Spec 8.5.4.4.2 Note: xsi:type+xsi:nil zusammen nur bei strict=true verboten
        let mut strict_opts = ExiOptions::default();
        strict_opts.strict = true;
        let result = encode_with_schema(&events_type_then_nil, &strict_opts, &schema);
        // Nach xsi:type Grammar-Switch kann xsi:nil als InvalidEventCode abgelehnt werden
        // (weil strip_xsi_type_nil die Production entfernt) oder als XsiTypeAndNilTogether.
        assert!(matches!(
            result,
            Err(Error::XsiTypeAndNilTogether)
                | Err(Error::InvalidEventCode { .. })
        ));

        // xsi:nil gefolgt von xsi:type - bei strict=true NICHT erlaubt
        let events_nil_then_type = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_nil()),
                value: "true".into(),
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "xs:string".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let result = encode_with_schema(&events_nil_then_type, &strict_opts, &schema);
        // XsiTypeAndNilTogether, XsiNilContentNotEmpty oder InvalidEventCode können auftreten.
        assert!(matches!(
            result,
            Err(Error::XsiTypeAndNilTogether)
                | Err(Error::XsiNilContentNotEmpty)
                | Err(Error::InvalidEventCode { .. })
        ));
    }

    // ========================================================================
    // Coverage-Tests
    // ========================================================================

    /// Test für Prefix CompactId Hit (Zeile 843-845).
    ///
    /// Wenn ein Prefix mehrfach verwendet wird, sollte es beim zweiten Mal
    /// als Hit im String Table erkannt werden.
    #[test]
    fn prefix_compact_id_hit() {
        let mut options = ExiOptions::default();
        options.preserve.prefixes = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::with_prefix("http://example.org", "root", "ex"))),
            // Zweites Element mit gleichem Prefix - sollte Hit sein
            ExiEvent::StartElement(Rc::new(QName::with_prefix("http://example.org", "child", "ex"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Sollte ohne Fehler encodieren
        let result = encode(&events, &options);
        assert!(result.is_ok());
    }

    /// Test für xsi:type mit Schema aber Typ nicht gefunden (strict=false).
    ///
    /// Zeile 1031: Bei strict=false wird kein Fehler zurückgegeben.
    #[test]
    fn xsi_type_not_found_strict_false() {
        let schema = SchemaInfo::builder().build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "UnknownType".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut options = ExiOptions::default();
        options.strict = false;

        // Sollte ohne Fehler durchlaufen (strict=false)
        let result = encode_with_schema(&events, &options, &schema);
        assert!(result.is_ok());
    }

    /// Test für xsi:type ohne Schema (Zeile 1009).
    ///
    /// Wenn kein Schema vorhanden ist, wird xsi:type ignoriert.
    #[test]
    fn xsi_type_without_schema() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::xsi_type()),
                value: "xs:string".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // encode() ohne Schema
        let result = encode(&events, &ExiOptions::default());
        assert!(result.is_ok());
    }

    // ========================================================================
    // Self-Contained (Spec 8.5.4.4.1)
    // ========================================================================

    /// Dekodiert alle Events bis EndDocument (inkl.) und gibt sie zurueck.
    /// Panics bei Dekodierfehlern — nur fuer Tests geeignet.
    fn decode_all_events(data: &[u8], options: ExiOptions) -> Vec<ExiEvent> {
        let mut decoder =
            crate::decoder::Decoder::with_options(data, options).expect("SC decoder init");
        let mut events = Vec::new();
        loop {
            match decoder.decode_event().expect("SC decode must succeed") {
                Some(event) => {
                    let is_ed = matches!(event, ExiEvent::EndDocument);
                    events.push(event);
                    if is_ed {
                        break;
                    }
                }
                None => break,
            }
        }
        events
    }

    /// SC Round-Trip: SD → SE(root) → SE(child) → CH → EE → EE → ED
    /// Bei self_contained=true wird das Kind-Element als SC-Fragment encodiert
    /// und kann vom Decoder korrekt gelesen werden.
    #[test]
    fn sc_round_trip_schema_less() {
        let mut options = ExiOptions::default();
        options.self_contained = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Characters(ChContent {
                value: "hello".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        // Decoder mit gleichen Optionen
        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC mit verschachtelten Kind-Elementen.
    #[test]
    fn sc_nested_children() {
        let mut options = ExiOptions::default();
        options.self_contained = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::Characters(ChContent {
                value: "1".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "b"))),
            ExiEvent::Characters(ChContent {
                value: "2".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC State-Isolation: String Table innerhalb SC hat keine Auswirkung auf
    /// den äußeren State.
    #[test]
    fn sc_state_isolation() {
        let mut options = ExiOptions::default();
        options.self_contained = true;

        // "child" Element mit langen String-Values innerhalb SC
        // Nach SC wird ein neues Element "other" encodiert — die String Table
        // darf den SC-internen State nicht behalten.
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Characters(ChContent {
                value: "sc_internal_value".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "other"))),
            ExiEvent::Characters(ChContent {
                value: "outer_value".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC Verschachtelt: Root → child1 (SC) → child2 (SC innerhalb SC)
    #[test]
    fn sc_nested_two_levels() {
        let mut options = ExiOptions::default();
        options.self_contained = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child1"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child2"))),
            ExiEvent::Characters(ChContent {
                value: "deep".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC mit ByteAlignment: Verifiziert dass align_to_byte() an SC-Grenzen
    /// korrekt funktioniert (Spec 8.5.4.4.1).
    #[test]
    fn sc_round_trip_byte_aligned() {
        let mut options = ExiOptions::default();
        options.self_contained = true;
        options.alignment = Alignment::ByteAlignment;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Characters(ChContent {
                value: "hello".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC QNames-Filter: Nur Elemente mit passendem QName werden SC-gewrappt
    /// (Spec 8.5.4.4.1, Application-Level Filterung).
    #[test]
    fn sc_qnames_filter_wraps_matching() {
        let mut options = ExiOptions::default();
        options.self_contained = true;
        options.self_contained_qnames = vec![QName::new("", "child")];

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Characters(ChContent {
                value: "wrapped".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "other"))),
            ExiEvent::Characters(ChContent {
                value: "not_wrapped".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes_filtered = encoder.finish().unwrap();

        // Vergleich: Ohne Filter (alle SC-gewrappt) muss anders sein
        let mut options_all = ExiOptions::default();
        options_all.self_contained = true;

        let mut encoder_all = Encoder::new(options_all, EncoderConfig::default()).unwrap();
        for event in &events {
            encoder_all.encode_event(event).unwrap();
        }
        let bytes_all = encoder_all.finish().unwrap();

        // Gefiltert hat weniger SC-Overhead → kleiner oder zumindest anders
        assert_ne!(bytes_filtered, bytes_all);

        // Round-Trip mit gefilterter Variante muss korrekt decodieren
        let decoded = decode_all_events(&bytes_filtered, options);
        assert_eq!(decoded, events);
    }

    /// SC QNames-Filter leer → alle SE werden SC-gewrappt.
    /// Expliziter Test für should_self_contain mit leerer Liste.
    #[test]
    fn sc_qnames_filter_empty_wraps_all() {
        let mut options = ExiOptions::default();
        options.self_contained = true;
        // self_contained_qnames bleibt leer → alle Elemente

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "b"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        // Vergleich: Ohne SC muss kürzer sein (kein SC-Overhead)
        let no_sc_options = ExiOptions::default();
        let mut encoder_no_sc =
            Encoder::new(no_sc_options, EncoderConfig::default()).unwrap();
        for event in &events {
            encoder_no_sc.encode_event(event).unwrap();
        }
        let bytes_no_sc = encoder_no_sc.finish().unwrap();

        assert!(bytes.len() > bytes_no_sc.len(), "SC-Version muss größer sein als ohne SC");

        // Round-Trip
        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    /// SC mit Namespaces: Verifiziert dass String Table korrekt resetted wird
    /// und Namespace-URIs innerhalb des SC-Fragments isoliert sind (Spec 8.5.4.4.1).
    #[test]
    fn sc_with_namespaces() {
        let mut options = ExiOptions::default();
        options.self_contained = true;
        options.preserve.prefixes = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::with_prefix("urn:ns1", "root", "ns1"))),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "urn:ns1".into(),
                prefix: "ns1".into(),
                local_element_ns: true,
            }),
            ExiEvent::StartElement(Rc::new(QName::with_prefix("urn:ns1", "child", "ns1"))),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "urn:ns1".into(),
                prefix: "ns1".into(),
                local_element_ns: true,
            }),
            ExiEvent::Characters(ChContent {
                value: "value".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    // ========================================================================
    // Enumeration Encoding (Spec 7.2)
    // ========================================================================

    /// Baut ein Schema mit einem Enum-Typ (rot/gruen/blau) auf Element "farbe".
    fn build_enum_schema() -> (crate::schema::SchemaInfo, Rc<crate::schema::TypeDefinition>) {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};

        let enum_type = Rc::new(TypeDefinition::Simple {
            name: None,
            variety: crate::schema::SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("string".into()),
            enumeration_values: vec!["rot".into(), "gruen".into(), "blau".into()],
            is_union: false,
            has_named_sub_types: false,
        });
        let elem_decl = Rc::new(
            ElementDeclaration::new(Rc::new(QName::new("", "farbe")))
                .with_type(enum_type.clone())
        );
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .element_declaration(Rc::new(QName::new("", "farbe")), elem_decl)
            .type_definition(Rc::new(QName::new("", "FarbTyp")), enum_type.clone())
            .build();
        (schema, enum_type)
    }

    /// Baut eine Event-Sequenz mit einem Characters-Wert im Element "farbe".
    fn enum_events(value: &str) -> Vec<ExiEvent> {
        vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "farbe"))),
            ExiEvent::Characters(ChContent { value: value.into() }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ]
    }

    /// Spec 7.2: Enumeration-Wert wird als n-Bit Index encodiert.
    /// Round-Trip: Encode → Decode mit Schema-Enum-Typ.
    #[test]
    fn enum_round_trip_strict() {
        let (schema, _) = build_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = enum_events("gruen");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        assert_eq!(events, decoded);
    }

    /// Spec 7.2: Enum-Fallback bei non-strict -- Wert nicht in Enum-Liste
    /// wird als String encodiert. Encoder darf keinen Fehler werfen.
    ///
    /// Hinweis: Round-Trip funktioniert hier nicht, da der Decoder Enum-Index
    /// liest wo der Encoder einen String geschrieben hat. Das ist eine bekannte
    /// Einschraenkung -- siehe BACKLOG.md (Value-Space Matching).
    #[test]
    fn enum_fallback_non_strict_encodes_without_error() {
        let (schema, _) = build_enum_schema();
        let options = ExiOptions { strict: false, ..Default::default() };
        let events = enum_events("gelb"); // nicht in Enum

        let result = crate::encode_with_schema(&events, &options, &schema);
        assert!(result.is_ok(), "Non-strict Enum-Fallback soll keinen Fehler werfen: {:?}", result.err());
    }

    /// Spec 7.2: Non-strict Enum Round-Trip mit gueltigem Wert.
    #[test]
    fn enum_non_strict_valid_value_round_trip() {
        let (schema, _) = build_enum_schema();
        let options = ExiOptions { strict: false, ..Default::default() };
        let events = enum_events("blau");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        assert_eq!(events, decoded);
    }

    /// Spec 7.2: Enum-Wert nicht in Liste + strict -> Fehler.
    #[test]
    fn enum_strict_unknown_value_error() {
        let (schema, _) = build_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = enum_events("gelb"); // nicht in Enum

        let result = crate::encode_with_schema(&events, &options, &schema);
        assert!(result.is_err());
        let err_msg = format!("{}", result.unwrap_err());
        assert!(err_msg.contains("nicht in Enumeration"), "Fehler soll Enum-Kontext haben: {err_msg}");
    }

    /// Spec 7.2 + 5.4: Bei Preserve.lexicalValues=true werden Enums
    /// als String encodiert statt als n-Bit Index.
    #[test]
    fn enum_lexical_values_preserves_as_string() {
        let (schema, _) = build_enum_schema();
        let options = ExiOptions {
            preserve: Preserve { lexical_values: true, ..Default::default() },
            ..Default::default()
        };
        let events = enum_events("gruen");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        assert_eq!(events, decoded);
    }

    // ========================================================================
    // Value-Space Enum Matching Tests (Spec 7.2)
    // ========================================================================

    /// Baut ein Schema mit Integer-Enum (Werte "0", "1", "2") auf Element "zahl".
    fn build_integer_enum_schema() -> crate::schema::SchemaInfo {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};

        let enum_type = Rc::new(TypeDefinition::Simple {
            name: None,
            variety: crate::schema::SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("integer".into()),
            enumeration_values: vec!["0".into(), "1".into(), "2".into()],
            is_union: false,
            has_named_sub_types: false,
        });
        let elem_decl = Rc::new(
            ElementDeclaration::new(Rc::new(QName::new("", "zahl")))
                .with_type(enum_type.clone())
        );
        SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .element_declaration(Rc::new(QName::new("", "zahl")), elem_decl)
            .type_definition(Rc::new(QName::new("", "IntEnumTyp")), enum_type)
            .build()
    }

    /// Baut ein Schema mit Boolean-Enum (Werte "true", "false") auf Element "flag".
    fn build_boolean_enum_schema() -> crate::schema::SchemaInfo {
        use crate::schema::{SchemaInfo, ElementDeclaration, TypeDefinition};

        let enum_type = Rc::new(TypeDefinition::Simple {
            name: None,
            variety: crate::schema::SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("boolean".into()),
            enumeration_values: vec!["true".into(), "false".into()],
            is_union: false,
            has_named_sub_types: false,
        });
        let elem_decl = Rc::new(
            ElementDeclaration::new(Rc::new(QName::new("", "flag")))
                .with_type(enum_type.clone())
        );
        SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .element_declaration(Rc::new(QName::new("", "flag")), elem_decl)
            .type_definition(Rc::new(QName::new("", "BoolEnumTyp")), enum_type)
            .build()
    }

    fn integer_enum_events(value: &str) -> Vec<ExiEvent> {
        vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "zahl"))),
            ExiEvent::Characters(ChContent { value: value.into() }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ]
    }

    fn boolean_enum_events(value: &str) -> Vec<ExiEvent> {
        vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "flag"))),
            ExiEvent::Characters(ChContent { value: value.into() }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ]
    }

    /// Spec 7.2: Integer Enum Value-Space — "+1" matcht "1" im Schema.
    #[test]
    fn enum_value_space_integer_plus_prefix() {
        let schema = build_integer_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = integer_enum_events("+1");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        // Decoder gibt den Schema-Wert zurück (Index 1 = "1"), nicht "+1"
        let expected = integer_enum_events("1");
        assert_eq!(decoded, expected);
    }

    /// Spec 7.2: Integer Enum Value-Space — "-0" matcht "0" im Schema.
    #[test]
    fn enum_value_space_integer_negative_zero() {
        let schema = build_integer_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = integer_enum_events("-0");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        let expected = integer_enum_events("0");
        assert_eq!(decoded, expected);
    }

    /// Spec 7.2: Integer Enum Value-Space — "01" matcht "1" im Schema.
    #[test]
    fn enum_value_space_integer_leading_zero() {
        let schema = build_integer_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = integer_enum_events("01");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        let expected = integer_enum_events("1");
        assert_eq!(decoded, expected);
    }

    /// Spec 7.2: Boolean Enum Value-Space — "1" matcht "true" im Schema.
    #[test]
    fn enum_value_space_boolean_one() {
        let schema = build_boolean_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = boolean_enum_events("1");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        let expected = boolean_enum_events("true");
        assert_eq!(decoded, expected);
    }

    /// Spec 7.2: Boolean Enum Value-Space — "0" matcht "false" im Schema.
    #[test]
    fn enum_value_space_boolean_zero() {
        let schema = build_boolean_enum_schema();
        let options = ExiOptions { strict: true, ..Default::default() };
        let events = boolean_enum_events("0");

        let encoded = crate::encode_with_schema(&events, &options, &schema).unwrap();
        let (decoded, _) = crate::decode_with_schema(&encoded, options.clone(), &schema).unwrap();
        let expected = boolean_enum_events("false");
        assert_eq!(decoded, expected);
    }

    /// SC drei Ebenen tief: Root → child1 (SC) → child2 (SC) → child3 (SC).
    /// Testet State-Stack-Tiefe > 2 (Spec 8.5.4.4.1).
    #[test]
    fn sc_nested_three_levels() {
        let mut options = ExiOptions::default();
        options.self_contained = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "l1"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "l2"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "l3"))),
            ExiEvent::Characters(ChContent {
                value: "leaf".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let mut encoder = Encoder::new(options.clone(), EncoderConfig::default()).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let bytes = encoder.finish().unwrap();

        let decoded = decode_all_events(&bytes, options);
        assert_eq!(decoded, events);
    }

    // ========================================================================
    // Streaming Whitespace-Filterung (classify_whitespace / pending_ch)
    // ========================================================================

    /// Helper: Element-only Schema mit <root><child/></root>.
    fn element_only_schema() -> crate::schema::SchemaInfo {
        let xsd = r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="child" type="xs:string"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"#;
        crate::xsd::parse_xsd(xsd).unwrap()
    }

    /// Streaming: CH(ws) + EE in element-only → simple data → Whitespace beibehalten.
    /// WS muss auf der element-only Ebene sein (root), nicht inside simple type (child).
    #[test]
    fn streaming_ws_before_ee_is_kept() {
        let schema = element_only_schema();
        let mut encoder = Encoder::with_schema(
            ExiOptions::default(), EncoderConfig::default(), schema,
        ).unwrap();

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "root")))).unwrap();
        // WS auf element-only Ebene, vor EE = simple data → NeedLookAhead
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: "  ".into() })).unwrap();
        assert!(encoder.pending_ch.is_some(), "WS sollte gepuffert sein (NeedLookAhead)");
        encoder.encode_event(&ExiEvent::EndElement).unwrap(); // EE → flush keep
        assert!(encoder.pending_ch.is_none());
        encoder.encode_event(&ExiEvent::EndDocument).unwrap();
        let bytes = encoder.finish().unwrap();

        // Decode und prüfen dass CH("  ") enthalten ist
        let (decoded, _) = crate::decoder::decode_with_schema(
            &bytes, ExiOptions::default(), &element_only_schema(),
        ).unwrap();
        assert!(
            decoded.iter().any(|e| matches!(e, ExiEvent::Characters(ch) if ch.value.as_ref() == "  ")),
            "WS vor EE sollte beibehalten werden: {decoded:?}",
        );
    }

    /// Streaming: CH(ws) + SE → complex data → Whitespace verwerfen.
    #[test]
    fn streaming_ws_before_se_is_skipped() {
        let schema = element_only_schema();
        let mut encoder = Encoder::with_schema(
            ExiOptions::default(), EncoderConfig::default(), schema,
        ).unwrap();

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "root")))).unwrap();
        // WS vor SE = complex data → skip
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: "\n  ".into() })).unwrap();
        assert!(encoder.pending_ch.is_some(), "WS sollte gepuffert sein");
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "child")))).unwrap(); // SE → flush skip
        assert!(encoder.pending_ch.is_none());
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: "val".into() })).unwrap();
        encoder.encode_event(&ExiEvent::EndElement).unwrap();
        encoder.encode_event(&ExiEvent::EndElement).unwrap();
        encoder.encode_event(&ExiEvent::EndDocument).unwrap();
        let bytes = encoder.finish().unwrap();

        let (decoded, _) = crate::decoder::decode_with_schema(
            &bytes, ExiOptions::default(), &element_only_schema(),
        ).unwrap();
        // WS "\n  " sollte NICHT enthalten sein
        assert!(
            !decoded.iter().any(|e| matches!(e, ExiEvent::Characters(ch) if ch.value.as_ref() == "\n  ")),
            "WS vor SE sollte verworfen werden: {decoded:?}",
        );
    }

    /// WS nach Kind-SE (in_content=true) wird sofort als Skip klassifiziert.
    #[test]
    fn streaming_ws_after_child_se_is_skipped_immediately() {
        let schema = element_only_schema();
        let mut encoder = Encoder::with_schema(
            ExiOptions::default(), EncoderConfig::default(), schema,
        ).unwrap();

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "root")))).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "child")))).unwrap();
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: "val".into() })).unwrap();
        encoder.encode_event(&ExiEvent::EndElement).unwrap();
        // WS nach Kind-SE → in_content=true → sofort Skip (kein Buffering)
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: "\n".into() })).unwrap();
        assert!(encoder.pending_ch.is_none(), "in_content WS sollte sofort geskippt werden");
        encoder.encode_event(&ExiEvent::EndElement).unwrap();
        encoder.encode_event(&ExiEvent::EndDocument).unwrap();
        let bytes = encoder.finish().unwrap();

        let (decoded, _) = crate::decoder::decode_with_schema(
            &bytes, ExiOptions::default(), &element_only_schema(),
        ).unwrap();
        assert!(
            !decoded.iter().any(|e| matches!(e, ExiEvent::Characters(ch) if ch.value.as_ref() == "\n")),
            "WS nach Kind-SE sollte verworfen werden: {decoded:?}",
        );
    }

    /// Unicode-Whitespace (NBSP U+00A0) wird NICHT als XML-WS behandelt → Keep.
    #[test]
    fn unicode_whitespace_not_filtered() {
        let schema = element_only_schema();
        let mut encoder = Encoder::with_schema(
            ExiOptions::default(), EncoderConfig::default(), schema,
        ).unwrap();

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "root")))).unwrap();
        // NBSP ist kein XML-WS → classify_whitespace → Keep (sofort)
        let nbsp = "\u{00A0}";
        encoder.encode_event(&ExiEvent::Characters(ChContent { value: nbsp.into() })).unwrap();
        assert!(encoder.pending_ch.is_none(), "NBSP sollte NICHT gepuffert werden");
    }

    /// WhitespaceAction::resolve() Semantik.
    #[test]
    fn whitespace_action_resolve() {
        assert_eq!(
            WhitespaceAction::NeedLookAhead.resolve(Some(&ExiEvent::EndElement)),
            WhitespaceAction::Keep,
        );
        assert_eq!(
            WhitespaceAction::NeedLookAhead.resolve(Some(&ExiEvent::StartElement(Rc::new(QName::new("", "x"))))),
            WhitespaceAction::Skip,
        );
        assert_eq!(
            WhitespaceAction::NeedLookAhead.resolve(None),
            WhitespaceAction::Skip,
        );
        assert_eq!(WhitespaceAction::Keep.resolve(None), WhitespaceAction::Keep);
        assert_eq!(WhitespaceAction::Skip.resolve(None), WhitespaceAction::Skip);
    }

    /// Batch- und Streaming-Pfad liefern identisches Ergebnis.
    #[test]
    fn batch_and_streaming_produce_identical_output() {
        let schema = element_only_schema();
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "\n  ".into() }),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Characters(ChContent { value: "hello".into() }),
            ExiEvent::EndElement,
            ExiEvent::Characters(ChContent { value: "\n".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Batch-Pfad
        let batch = encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();

        // Streaming-Pfad
        let mut encoder = Encoder::with_schema(
            ExiOptions::default(), EncoderConfig::default(), schema,
        ).unwrap();
        for event in &events {
            encoder.encode_event(event).unwrap();
        }
        let streaming = encoder.finish().unwrap();

        assert_eq!(batch, streaming, "Batch und Streaming sollten identische Bytes liefern");
    }

    // ========================================================================
    // Whitespace-Stripping (Schema-less)
    // ========================================================================

    /// Extrahiert alle CH-Werte aus einer Event-Sequenz.
    fn collect_ch_values(events: &[ExiEvent]) -> Vec<&str> {
        events.iter().filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(ch.value.as_ref()),
            _ => None,
        }).collect()
    }

    /// Einrueckungs-Whitespace zwischen Geschwister-Elementen wird gestrippt.
    /// WS vor dem ersten Kind ("\n  " vor <a>) bleibt — NeedLookAhead.
    /// WS nach Geschwistern ("\n  " zwischen </a> und <b>, "\n" vor </root>) wird gestrippt.
    #[test]
    fn ws_strip_indentation_between_siblings() {
        let xml = "<root>\n  <a>1</a>\n  <b>2</b>\n</root>";
        let opts = ExiOptions::default();
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let ch_values = collect_ch_values(&events);
        // Vor erstem Kind: "\n  " bleibt (NeedLookAhead), danach "1", "2"
        assert_eq!(ch_values, vec!["\n  ", "1", "2"]);
    }

    /// preserve.whitespace = true behaelt Einrueckungs-WS.
    #[test]
    fn ws_strip_preserve_whitespace_keeps_ws() {
        let xml = "<root>\n  <a>1</a>\n  <b>2</b>\n</root>";
        let opts = ExiOptions::default().with_preserve(Preserve {
            whitespace: true,
            ..Default::default()
        });
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let ch_values = collect_ch_values(&events);
        // Whitespace wird beibehalten
        assert!(ch_values.len() > 2, "WS-Events muessen erhalten bleiben");
        assert!(ch_values.iter().any(|v| v.trim().is_empty()), "WS-only Events erwartet");
    }

    /// preserve.lexical_values = true impliziert WS-Erhalt.
    #[test]
    fn ws_strip_lexical_values_implies_ws_preserve() {
        let xml = "<root>\n  <a/>\n  <b/>\n</root>";
        let opts = ExiOptions::default().with_preserve(Preserve {
            lexical_values: true,
            ..Default::default()
        });
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let ch_values = collect_ch_values(&events);
        assert!(ch_values.iter().any(|v| v.trim().is_empty()), "WS-only Events erwartet");
    }

    /// Nicht-WS-Text in Mixed Content wird nicht gestrippt.
    #[test]
    fn ws_strip_mixed_content_preserved() {
        let xml = "<root><a/>hello<b/>world</root>";
        let opts = ExiOptions::default();
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let ch_values = collect_ch_values(&events);
        assert_eq!(ch_values, vec!["hello", "world"]);
    }

    /// WS vor dem ersten Kind-Element wird nicht frueh gefiltert (NeedLookAhead).
    #[test]
    fn ws_strip_before_first_child_not_early_filtered() {
        let xml = "<root>\n  <a/>\n</root>";
        let opts = ExiOptions::default();
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let ch_count = events.iter().filter(|e| matches!(e, ExiEvent::Characters(_))).count();
        // Vor dem ersten Kind: NeedLookAhead → Encoder entscheidet.
        // Nach dem Kind ("\n"): wird frueh gefiltert (had_child_se).
        // Genau 0 oder 1 CH-Events (Encoder-abhaengig).
        assert!(ch_count <= 1, "Hoechstens 1 CH-Event erwartet, bekam {ch_count}");
    }

    /// End-to-End: Encode+Decode Round-Trip mit WS-Stripping.
    #[test]
    fn ws_strip_encode_decode_roundtrip() {
        let xml = "<root>\n  <a>text</a>\n  <b>more</b>\n</root>";
        let opts = ExiOptions::default();
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let exi = encode(&events, &opts).unwrap();
        let (decoded, _) = crate::decoder::decode(&exi).unwrap();
        let ch_values = collect_ch_values(&decoded);
        assert!(ch_values.contains(&"text"));
        assert!(ch_values.contains(&"more"));
    }
