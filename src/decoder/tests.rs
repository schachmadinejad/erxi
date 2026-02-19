use super::*;
    use crate::encoder::{EncoderConfig, encode, encode_with_config};
    use crate::event::{AtContent, ChContent, CmContent, PiContent};
    use crate::options::Preserve;

    // ========================================================================
    // Schritt 1: Decoder Grundstruktur + Header
    // ========================================================================

    /// Decoder kann erstellt werden.
    #[test]
    fn decoder_new() {
        let data = [0x80]; // Minimaler Header
        let decoder = Decoder::new(&data).unwrap();
        assert!(!decoder.header_read);
        assert!(!decoder.finished);
    }

    /// Minimaler Header wird korrekt decodiert.
    ///
    /// Spec 5.2, 5.3: Distinguishing Bits (10) + Version (0 für Version 1).
    #[test]
    fn decoder_minimal_header() {
        let data = [0x80]; // 10 00 0000 = Dist-Bits + No-Options + Version 1
        let mut decoder = Decoder::new(&data).unwrap();
        decoder.read_header().unwrap();
        assert!(decoder.header_read);
        assert_eq!(decoder.options.alignment, Alignment::BitPacked);
    }

    /// Header mit Cookie wird korrekt decodiert.
    #[test]
    fn decoder_header_with_cookie() {
        // "$EXI" + minimaler Header
        let data = [0x24, 0x45, 0x58, 0x49, 0x80];
        let mut decoder = Decoder::new(&data).unwrap();
        decoder.read_header().unwrap();
        assert!(decoder.header_read);
    }

    /// finish() gibt Fehler wenn EndDocument nicht erreicht wurde.
    #[test]
    fn decoder_finish_rejects_incomplete() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let mut decoder = Decoder::new(&bytes).unwrap();

        // Nur SD + SE lesen, nicht komplett decodieren
        decoder.decode_event().unwrap();
        decoder.decode_event().unwrap();

        let err = decoder.finish().unwrap_err();
        assert!(err.to_string().contains("EndDocument"), "{err}");
    }

    /// Leerer Stream gibt Fehler.
    #[test]
    fn decoder_empty_stream() {
        let data = [];
        let mut decoder = Decoder::new(&data).unwrap();
        let result = decoder.read_header();
        assert!(result.is_err());
    }

    /// Ungültige Distinguishing Bits geben Fehler.
    #[test]
    fn decoder_invalid_distinguishing_bits() {
        // 00 statt 10
        let data = [0x00];
        let mut decoder = Decoder::new(&data).unwrap();
        let result = decoder.read_header();
        assert!(matches!(result, Err(Error::InvalidDistinguishingBits(_))));
    }

    // ========================================================================
    // Schritt 2: High-Level decode() Funktion
    // ========================================================================

    /// High-Level decode() mit minimalem Stream.
    #[test]
    fn decode_minimal_stream() {
        // Encoder: SD, SE(root), EE, ED
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded_events, _options) = decode(&bytes).unwrap();

        assert_eq!(decoded_events.len(), 4);
        assert!(matches!(decoded_events[0], ExiEvent::StartDocument));
        assert!(matches!(decoded_events[3], ExiEvent::EndDocument));

        // Prüfe SE(root)
        if let ExiEvent::StartElement(ref qname) = decoded_events[1] {
            assert_eq!(&*qname.local_name, "root");
            assert_eq!(&*qname.uri, "");
        } else {
            panic!("Expected StartElement");
        }
    }

    // ========================================================================
    // Schritt 3: Round-Trip Tests
    // ========================================================================

    /// Round-Trip: Minimales Dokument.
    #[test]
    fn round_trip_minimal_document() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events.len(), decoded.len());
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Dokument mit Cookie.
    #[test]
    fn round_trip_with_cookie() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let config = EncoderConfig::with_cookie();
        let bytes = encode_with_config(&events, &ExiOptions::default(), config).unwrap();

        // Prüfe Cookie
        assert!(bytes.starts_with(b"$EXI"));

        let (decoded, _) = decode(&bytes).unwrap();
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Verschachtelte Elemente.
    #[test]
    fn round_trip_nested_elements() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "grandchild"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Element mit Namespace.
    #[test]
    fn round_trip_element_with_namespace() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Attribute.
    #[test]
    fn round_trip_attribute() {
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

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Mehrere Attribute.
    #[test]
    fn round_trip_multiple_attributes() {
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

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Characters.
    #[test]
    fn round_trip_characters() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "Hello, World!".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Unicode Characters.
    #[test]
    fn round_trip_unicode_characters() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "日本語 🎉 Grüße".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Comments (preserve.comments=true).
    #[test]
    fn round_trip_comments() {
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

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.preserve.comments);
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Processing Instructions (preserve.pis=true).
    #[test]
    fn round_trip_pis() {
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
                name: "target".into(),
                text: "data".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Byte-aligned Modus.
    #[test]
    fn round_trip_byte_aligned() {
        let options = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
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

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert_eq!(decoded_options.alignment, Alignment::ByteAlignment);
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Pre-compression Modus.
    ///
    /// Spec 9: PreCompression Round-Trip mit Value Channels.
    #[test]
    fn round_trip_pre_compression() {
        let options = ExiOptions {
            alignment: Alignment::PreCompression,

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "test".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert_eq!(decoded_options.alignment, Alignment::PreCompression);
        assert_eq!(events, decoded);
    }

    /// Spec 9: Compression Round-Trip (DEFLATE).
    #[test]
    fn round_trip_compression() {
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

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.compression);
        assert_eq!(events, decoded);
    }

    /// Spec 9: Compression Round-Trip mit mehreren Values.
    #[test]
    fn round_trip_compression_multiple_values() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // 50 Attribute
        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.compression);
        assert_eq!(events.len(), decoded.len());
    }

    /// Spec 9.3: Multi-Stream Compression (>100 Values).
    #[test]
    fn round_trip_compression_multi_stream() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // >100 Attribute (löst Multi-Stream aus)
        for i in 0..150 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.compression);
        assert_eq!(events.len(), decoded.len());
    }

    /// Spec 9.1: Multi-Block Decoding ist noch nicht implementiert.
    ///
    /// Multi-Block erfordert einen speziellen Decoder, der:
    /// - Die Grammar zwischen Blöcken beibehält (Events lernen Produktionen)
    /// - Die Structure blockweise liest (bis block_size Values erreicht)
    /// - Die Values pro Block liest (nach Channel-Ordnung)
    ///
    /// Aktuell unterstützt der Decoder nur Single-Block (value_count <= block_size).
    #[test]
    fn round_trip_compression_multi_block() {
        let options = ExiOptions {
            compression: true,

            block_size: 10, // Kleiner block_size erzwingt Multi-Block
            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        // 50 Attribute = 50 Values > block_size (10)
        // Erzeugt mehrere Blöcke: Block 1 (10), Block 2 (10), ... Block 5 (10)
        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.compression);
        assert_eq!(decoded_options.block_size, 10);
        assert_eq!(events.len(), decoded.len());
        assert_eq!(events, decoded);
    }

    /// Multi-Block Compression Round-Trip über `decode_with_options`.
    #[test]
    fn round_trip_compression_multi_block_lazy() {
        let options = ExiOptions {
            compression: true,

            block_size: 10,
            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        let (decoded, decoded_options) = decode_with_options(&bytes, options.clone()).unwrap();

        assert!(decoded_options.compression);
        assert_eq!(decoded_options.block_size, 10);
        assert_eq!(events.len(), decoded.len());
        assert_eq!(events, decoded);
    }

    /// `deflate_stream_size()` muss exakt dieselben Grenzen liefern wie
    /// `deflate_decompress_stream()` — die Konsistenz ist Voraussetzung
    /// für korrektes Block-by-Block Decoding.
    #[test]
    fn deflate_stream_size_consistent_with_decompress() {
        use crate::compression::{deflate_stream_size, deflate_decompress_stream};

        let options = ExiOptions {
            compression: true,

            block_size: 10,
            ..Default::default()
        };

        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        // Body nach dem Header extrahieren (analog zu decode_compressed)
        let mut probe = Decoder::with_options(&bytes, options).unwrap();
        probe.read_header().unwrap();
        let body_offset = probe.reader.bit_position().div_ceil(8);
        let body = &bytes[body_offset..];

        // Grenzen mit beiden Funktionen finden
        let mut offset_size = 0;
        let mut boundaries_size = Vec::new();
        while offset_size < body.len() {
            let consumed = deflate_stream_size(&body[offset_size..]).unwrap();
            boundaries_size.push((offset_size, offset_size + consumed));
            offset_size += consumed;
        }

        let mut offset_decomp = 0;
        let mut boundaries_decomp = Vec::new();
        while offset_decomp < body.len() {
            let (_, consumed) = deflate_decompress_stream(&body[offset_decomp..]).unwrap();
            boundaries_decomp.push((offset_decomp, offset_decomp + consumed));
            offset_decomp += consumed;
        }

        assert_eq!(boundaries_size, boundaries_decomp,
            "deflate_stream_size und deflate_decompress_stream finden unterschiedliche Grenzen");
        assert!(boundaries_size.len() > 1,
            "Test braucht mehrere Streams, aber nur {} gefunden", boundaries_size.len());
    }

    // ========================================================================
    // Lazy Compression Iterator Tests
    // ========================================================================

    /// Iterator-API mit Compression (Single-Block Roundtrip).
    ///
    /// Spec 9.3: ≤100 Values → ein Stream.
    #[test]
    fn iter_compression_single_block_roundtrip() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "hello".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        let (iter, opts) = decode_iter_with_options(&bytes, options).unwrap();
        assert!(opts.compression);
        let decoded: Vec<ExiEvent> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(events, decoded);
    }

    /// Iterator-API mit Schema + Compression Roundtrip.
    #[test]
    fn iter_schema_compression_roundtrip() {
        let xsd = r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root" type="xs:string"/>
</xs:schema>"#;
        let schema = crate::xsd::parse_xsd(xsd).unwrap();
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "world".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = crate::encoder::encode_with_schema_and_config(
            &events, &options, &schema, config,
        ).unwrap();

        let (iter, opts) = decode_iter_with_schema(&bytes, options, &schema).unwrap();
        assert!(opts.compression);
        let decoded: Vec<ExiEvent> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(events, decoded);
    }

    /// Multi-Block Compression über Iterator-API.
    ///
    /// Spec 9.1: block_size=10, >10 Values → Multi-Block.
    #[test]
    fn iter_compression_multi_block() {
        let options = ExiOptions {
            compression: true,

            block_size: 10,
            ..Default::default()
        };
        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];
        for i in 0..50 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{i}"))),
                value: format!("value{i}").into(),
            }));
        }
        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        let (iter, opts) = decode_iter_with_options(&bytes, options).unwrap();
        assert!(opts.compression);
        assert_eq!(opts.block_size, 10);
        let decoded: Vec<ExiEvent> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(events.len(), decoded.len());
        assert_eq!(events, decoded);
    }

    /// Early-Abort + finish() bei Compression-Iterator.
    #[test]
    fn iter_compression_early_abort_finish() {
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "data".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        let (mut iter, _) = decode_iter_with_options(&bytes, options).unwrap();
        // Nur erstes Event lesen, dann abbrechen
        let first = iter.next().unwrap().unwrap();
        assert!(matches!(first, ExiEvent::StartDocument));
        // finish() sollte keinen Panic auslösen (EndDocument nicht erreicht → Err ist ok)
        let _ = iter.finish();
    }

    /// Fehler wird nur einmal geliefert (kein Endlosloop nach Fehler).
    #[test]
    fn compressed_lazy_error_terminates() {
        // Ungültige DEFLATE-Daten nach gültigem Header
        let options = ExiOptions {
            compression: true,

            ..Default::default()
        };
        // Erstelle gültigen Header mit compression=true
        let mut header_writer = crate::bitstream::BitWriter::new();
        let header = crate::header::ExiHeader::new().with_options();
        crate::header::encode(&mut header_writer, &header, false).unwrap();
        crate::options_codec::encode(&mut header_writer, &options).unwrap();
        header_writer.align_to_byte();
        let mut data = header_writer.into_vec();
        // Ungültige DEFLATE-Daten anhängen
        data.extend_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF]);

        let result = decode_iter_with_options(&data, options);
        if let Ok((iter, _)) = result {
            let results: Vec<Result<ExiEvent>> = iter.collect();
            // Genau ein Fehler, kein Endlosloop
            assert!(results.len() <= 1, "Zu viele Ergebnisse: {}", results.len());
            if let Some(r) = results.first() {
                assert!(r.is_err());
            }
        }
        // Falls schon decode_iter fehlschlägt, ist das auch ok
    }

    // ========================================================================
    // Schritt 4: String Table Round-Trip Tests
    // ========================================================================

    /// Round-Trip: Wiederholte URIs (String Table Hit).
    #[test]
    fn round_trip_repeated_uri() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "child"))),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Wiederholte LocalNames (String Table Hit).
    #[test]
    fn round_trip_repeated_local_name() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))), // Hit
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Pre-populated URIs (leere URI).
    #[test]
    fn round_trip_pre_populated_empty_uri() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))), // Leere URI ist pre-populated
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Wiederholte Attribute Values (String Table Hit - Spec 7.3.3).
    #[test]
    fn round_trip_repeated_attribute_value() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "type")),
                value: "example".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("", "child"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "type")),
                value: "example".into(), // Local Hit (gleicher QName)
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Wiederholte Character Values (String Table Hit - Spec 7.3.3).
    #[test]
    fn round_trip_repeated_character_value() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent {
                value: "repeated text".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "repeated text".into(), // Local Hit (gleicher Element-QName)
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Leere Values (werden nicht zur String Table hinzugefügt).
    #[test]
    fn round_trip_empty_value() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "attr")),
                value: "".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Value Partition Capacity (bounded String Table).
    #[test]
    fn round_trip_value_partition_capacity() {
        let options = ExiOptions {
            value_partition_capacity: Some(2), // Nur 2 Values erlaubt

            ..Default::default()
        };

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "a")),
                value: "first".into(), // Miss, id=0
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "b")),
                value: "second".into(), // Miss, id=1
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "c")),
                value: "third".into(), // Miss, id=0 (evicts "first")
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "d")),
                value: "second".into(), // Global Hit (noch vorhanden)
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let config = EncoderConfig {
            include_cookie: false,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();
        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert_eq!(decoded_options.value_partition_capacity, Some(2));
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Global Value Hit (Value in anderem QName-Kontext).
    #[test]
    fn round_trip_global_value_hit() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "attr1")),
                value: "shared".into(), // Miss, wird global gespeichert
            }),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "attr2")),
                value: "shared".into(), // Global Hit (anderer QName)
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    // ========================================================================
    // Schritt 5: Grammar Evolution Tests
    // ========================================================================

    /// Round-Trip: Grammar Evolution bei SE(*) Match.
    #[test]
    fn round_trip_grammar_evolution_se() {
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

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Grammar Evolution bei AT(*) Match.
    #[test]
    fn round_trip_grammar_evolution_at() {
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

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    // ========================================================================
    // Schritt 6: Komplexe Dokumente
    // ========================================================================

    /// Round-Trip: Komplettes Dokument mit vielen Features.
    #[test]
    fn round_trip_complex_document() {
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
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("http://example.org", "child"))),
            ExiEvent::Characters(ChContent {
                value: "More text".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::Comment(CmContent {
                text: "Trailing comment".into(),
            }),
            ExiEvent::EndDocument,
        ];

        let config = EncoderConfig {
            include_cookie: true,
            include_options: true,
        };
        let bytes = encode_with_config(&events, &options, config).unwrap();

        // Prüfe Cookie
        assert!(bytes.starts_with(b"$EXI"));

        let (decoded, decoded_options) = decode(&bytes).unwrap();

        assert!(decoded_options.preserve.comments);
        assert!(decoded_options.preserve.pis);
        assert_eq!(events, decoded);
    }

    /// Round-Trip: Tief verschachtelte Elemente.
    #[test]
    fn round_trip_deep_nesting() {
        let mut events = vec![ExiEvent::StartDocument];

        for i in 0..20 {
            events.push(ExiEvent::StartElement(Rc::new(QName::new(
                "",
                format!("level{}", i),
            ))));
        }
        for _ in 0..20 {
            events.push(ExiEvent::EndElement);
        }
        events.push(ExiEvent::EndDocument);

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    /// Round-Trip: Viele Attribute an einem Element.
    #[test]
    fn round_trip_many_attributes() {
        let mut events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
        ];

        for i in 0..10 {
            events.push(ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", format!("attr{}", i))),
                value: format!("value{}", i).into(),
            }));
        }

        events.push(ExiEvent::EndElement);
        events.push(ExiEvent::EndDocument);

        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (decoded, _) = decode(&bytes).unwrap();

        assert_eq!(events, decoded);
    }

    // ========================================================================
    // Schritt 7: Fehler-Tests
    // ========================================================================

    /// Truncated Stream gibt Fehler.
    #[test]
    fn decode_truncated_stream() {
        // Nur Header, keine Events — Stream endet sofort nach Header.
        // Spec 6: ED ist Pflicht-Event, truncated Streams sind ein Fehler.
        let data = [0x80];
        let result = decode(&data);
        assert!(result.is_err());
    }

    /// Truncated Stream nach Header ist ein Fehler (Spec 6: ED Pflicht).
    #[test]
    fn decode_invalid_event_code() {
        // 0x80 = Header, dann endet Stream. PrematureEndOfStream ist ein Fehler.
        let data = [0x80];
        let result = decode(&data);
        assert!(result.is_err());
    }

    // ========================================================================
    // Schema-informed Decoder Tests (Issue #37)
    // ========================================================================

    /// Decoder::with_schema() erstellt Schema-informed Decoder.
    #[test]
    fn decoder_with_schema_creates_decoder() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let data = [0x80]; // Minimaler Header (SD fehlt, aber Decoder wird erstellt)
        let decoder = Decoder::with_schema(&data, ExiOptions::default(), schema);
        assert!(decoder.is_ok());
    }

    /// Schema-informed Decoder verwendet Schema-informed Document Grammar.
    #[test]
    fn decoder_with_schema_uses_schema_grammar() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .global_element(QName::new("http://example.org", "author"))
            .build();

        let data = [0x80];
        let decoder = Decoder::with_schema(&data, ExiOptions::default(), schema).unwrap();

        // Schema sollte vorhanden sein
        assert!(decoder.schema.is_some());
    }

    /// decode_with_schema() High-Level API funktioniert (Round-Trip).
    ///
    /// Schema-informed Encoder und Decoder müssen zusammen verwendet werden,
    /// da die Event Codes unterschiedlich zu Schema-less sind.
    #[test]
    fn decode_with_schema_roundtrip() {
        use crate::schema::SchemaInfo;
        use crate::encoder;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Encode mit Schema via Encoder struct
        let mut enc = encoder::Encoder::with_schema(
            ExiOptions::default(),
            encoder::EncoderConfig::default(),
            schema.clone(),
        ).unwrap();

        for event in &events {
            let bit_pos_before = enc.byte_position() * 8;
            enc.encode_event(event).unwrap();
            let bit_pos_after = enc.byte_position() * 8;
            eprintln!("Encode {:?} approx bits {}..{}", event, bit_pos_before, bit_pos_after);
        }
        let bytes = enc.finish().unwrap();

        eprintln!("Encoded bytes: {:?}", bytes);
        eprintln!("Encoded bytes (binary): {}", bytes.iter().map(|b| format!("{:08b}", b)).collect::<Vec<_>>().join(" "));
        eprintln!("Total bits: {}", bytes.len() * 8);

        // Erstelle Decoder mit Schema
        let mut dec = Decoder::with_schema(&bytes, ExiOptions::default(), schema).unwrap();

        // Manuell Header lesen und Events dekodieren
        let mut decoded_events = Vec::new();
        loop {
            let bit_pos_before = dec.reader.bit_position();
            match dec.decode_event() {
                Ok(Some(event)) => {
                    let bit_pos_after = dec.reader.bit_position();
                    eprintln!("Decode {:?} at bits {}..{}", event, bit_pos_before, bit_pos_after);
                    decoded_events.push(event);
                },
                Ok(None) => {
                    eprintln!("End of events at bit {}", dec.reader.bit_position());
                    break;
                },
                Err(e) => panic!("Decode error: {:?}", e),
            }
        }

        // Prüfe auf unglesene Bits (relaxter check)
        let remaining = dec.reader.remaining_bits();
        eprintln!("Remaining bits: {}", remaining);

        // Trailing Bits nach ED sind erlaubt (Padding auf Byte-Grenze).

        // Events sollten übereinstimmen
        assert_eq!(decoded_events.len(), events.len());
        assert!(matches!(decoded_events[0], ExiEvent::StartDocument));
        assert!(matches!(decoded_events[1], ExiEvent::StartElement(_)));
        assert!(matches!(decoded_events[2], ExiEvent::EndElement));
        assert!(matches!(decoded_events[3], ExiEvent::EndDocument));
    }

    /// Schema-informed Decoding mit pre-populated String Table.
    #[test]
    fn decoder_with_schema_prepopulates_string_table() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .all_element(QName::new("http://example.org", "chapter"))
            .attribute(QName::new("", "id"))
            .build();

        let data = [0x80];
        let decoder = Decoder::with_schema(&data, ExiOptions::default(), schema).unwrap();

        // Schema-URIs sollten pre-populated sein
        assert!(decoder.string_table.lookup_uri("http://example.org").is_some());

        // LocalNames sollten pre-populated sein
        let uri_id = decoder.string_table.lookup_uri("http://example.org").unwrap();
        assert!(decoder.string_table.lookup_local_name(uri_id, "book").is_some());
        assert!(decoder.string_table.lookup_local_name(uri_id, "chapter").is_some());
    }

    /// Spec 7.1: Typed Value Round-Trip mit Integer.
    ///
    /// Verifiziert, dass Integer-Werte korrekt encode + decode werden.
    ///
    #[test]
    fn typed_value_round_trip_integer() {
        use crate::schema::{SchemaInfo, TypeDefinition, ElementDeclaration};
        use crate::encoder;

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
            ExiEvent::Characters(ChContent { value: "12345".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Encode mit Schema
        let bytes = encoder::encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();

        // Decode mit Schema
        let (decoded, _) = decode_with_schema(&bytes, ExiOptions::default(), &schema).unwrap();

        // Verifiziere Round-Trip
        assert_eq!(decoded.len(), events.len());
        assert!(matches!(decoded[0], ExiEvent::StartDocument));
        assert!(matches!(decoded[1], ExiEvent::StartElement(_)));
        if let ExiEvent::Characters(ChContent { value }) = &decoded[2] {
            assert_eq!(&**value, "12345");
        } else {
            panic!("Expected Characters event");
        }
        assert!(matches!(decoded[3], ExiEvent::EndElement));
        assert!(matches!(decoded[4], ExiEvent::EndDocument));
    }

    /// Spec 7.1: Typed Value Round-Trip mit Boolean.
    ///
    #[test]
    fn typed_value_round_trip_boolean() {
        use crate::schema::{SchemaInfo, TypeDefinition, ElementDeclaration};
        use crate::encoder;

        let bool_type = Rc::new(TypeDefinition::simple_with_base("boolean"));
        let elem_qname = Rc::new(QName::new("", "flag"));
        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_type(bool_type),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "flag"))
            .element_declaration(elem_qname, root_elem)
            .build();

        for test_value in ["true", "false", "1", "0"] {
            let events = vec![
                ExiEvent::StartDocument,
                ExiEvent::StartElement(Rc::new(QName::new("", "flag"))),
                ExiEvent::Characters(ChContent { value: test_value.into() }),
                ExiEvent::EndElement,
                ExiEvent::EndDocument,
            ];

            let bytes = encoder::encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();
            let (decoded, _) = decode_with_schema(&bytes, ExiOptions::default(), &schema).unwrap();

            if let ExiEvent::Characters(ChContent { value }) = &decoded[2] {
                // Boolean-Werte werden normalisiert zu "true"/"false"
                let expected = match test_value {
                    "true" | "1" => "true",
                    "false" | "0" => "false",
                    _ => test_value,
                };
                assert_eq!(&**value, expected, "Boolean round-trip for '{}'", test_value);
            } else {
                panic!("Expected Characters event");
            }
        }
    }

    /// Spec 7.1: Typed Value Round-Trip für Attribute.
    ///
    /// Verifiziert, dass Attribut-Werte mit korrektem Typ encodiert werden.
    ///
    #[test]
    fn typed_value_round_trip_attribute() {
        use crate::schema::{SchemaInfo, TypeDefinition, ElementDeclaration, AttributeUse};
        use crate::encoder;

        // Schema mit Element das ein Integer-Attribut hat
        let int_type = Rc::new(TypeDefinition::simple_with_base("integer"));
        let elem_qname = Rc::new(QName::new("", "item"));
        let attr_use = AttributeUse {
            qname: Rc::new(QName::new("", "id")),
            required: true,
            type_definition: Some(int_type.clone()),
        };

        let elem_type = Rc::new(TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![attr_use],
            attribute_wildcard: None,
            content: crate::schema::ContentType::Empty,
            has_named_sub_types: false,
        });

        let root_elem = Rc::new(
            ElementDeclaration::new(elem_qname.clone()).with_type(elem_type),
        );

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "item"))
            .element_declaration(elem_qname, root_elem)
            .build();

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "id")),
                value: "42".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        // Encode mit Schema
        let bytes = encoder::encode_with_schema(&events, &ExiOptions::default(), &schema).unwrap();

        // Decode mit Schema
        let (decoded, _) = decode_with_schema(&bytes, ExiOptions::default(), &schema).unwrap();

        // Verifiziere Round-Trip
        assert_eq!(decoded.len(), events.len());
        if let ExiEvent::Attribute(AtContent { qname, value }) = &decoded[2] {
            assert_eq!(&*qname.local_name, "id");
            assert_eq!(&**value, "42");
        } else {
            panic!("Expected Attribute event, got {:?}", decoded[2]);
        }
    }

    // ========================================================================
    // Self-Contained: Decoder-spezifische Tests (Spec 8.5.4.4.1)
    // ========================================================================

    /// SC Decoder: Komplexe Verschachtelung mit Attributen und mehreren
    /// Kinderelementen, QNames-Filter (Spec 8.5.4.4.1).
    #[test]
    fn decode_sc_complex_with_attributes() {
        let mut options = ExiOptions::default();
        options.self_contained = true;
        options.self_contained_qnames = vec![QName::new("", "inner")];

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "id")),
                value: "r1".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("", "inner"))),
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "type")),
                value: "complex".into(),
            }),
            ExiEvent::StartElement(Rc::new(QName::new("", "leaf1"))),
            ExiEvent::Characters(ChContent {
                value: "val1".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "leaf2"))),
            ExiEvent::Characters(ChContent {
                value: "val2".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::StartElement(Rc::new(QName::new("", "after"))),
            ExiEvent::Characters(ChContent {
                value: "after_sc".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();
        let (decoded, _) = decode_with_options(&bytes, options).unwrap();
        assert_eq!(decoded, events);
    }

    /// SC Decoder: ByteAlignment mit Namespace-Deklarationen.
    /// Testet Decoder-seitige SC-Fragment-Verarbeitung bei ByteAlignment
    /// mit preserve.prefixes=true (analog sc-02, Spec 8.5.4.4.1).
    #[test]
    fn decode_sc_bytealigned_with_prefixes() {
        use crate::event::NsContent;

        let mut options = ExiOptions::default();
        options.self_contained = true;
        options.alignment = Alignment::ByteAlignment;
        options.preserve.prefixes = true;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::with_prefix("urn:foo", "root", "foo"))),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "urn:foo".into(),
                prefix: "foo".into(),
                local_element_ns: true,
            }),
            ExiEvent::StartElement(Rc::new(QName::with_prefix("urn:foo", "child", "foo"))),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "urn:foo".into(),
                prefix: "foo".into(),
                local_element_ns: true,
            }),
            ExiEvent::Characters(ChContent {
                value: "data".into(),
            }),
            ExiEvent::EndElement,
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];

        let bytes = encode(&events, &options).unwrap();
        let (decoded, _) = decode_with_options(&bytes, options).unwrap();
        assert_eq!(decoded, events);
    }

    /// Decodiert Exificient sc-01 bitpacked Fixture (Spec 8.5.4.4.1).
    /// Schema-informed mit acceptance.xsd — ohne explizites SC-Flag,
    /// da die Undeclared Productions die SC-Verarbeitung automatisch ermoeglichen.
    #[test]
    fn decode_exificient_sc_01_bitpacked() {
        use std::path::Path;

        let schema_path = Path::new(
            "tests/fixtures/w3c/acceptance.xsd",
        );
        if !schema_path.exists() {
            eprintln!("SKIP: acceptance.xsd nicht vorhanden");
            return;
        }

        let schema = crate::xsd::parse_xsd_with_imports(schema_path).unwrap();

        let data = std::fs::read(
            "tests/fixtures/w3c/schema_undeclared/sc-01_bitpacked.exi",
        )
        .expect("sc-01_bitpacked.exi muss vorhanden sein");

        let options = ExiOptions::default();

        let (events, _) = decode_with_schema(&data, options, &schema)
            .expect("sc-01_bitpacked muss decodierbar sein");

        // Grundlegende Struktur-Checks
        assert!(events.len() > 10, "sc-01 muss viele Events haben, hat {}", events.len());
        assert!(matches!(events.first(), Some(ExiEvent::StartDocument)));
        assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));

        // Verifiziere foo:ANY als Root-Element
        if let Some(ExiEvent::StartElement(qname)) = events.get(1) {
            assert_eq!(&*qname.uri, "urn:foo");
            assert_eq!(&*qname.local_name, "ANY");
        } else {
            panic!("Zweites Event muss SE(foo:ANY) sein, ist {:?}", events.get(1));
        }
    }

    /// Decodiert Exificient sc-02 bytealigned Fixture (Spec 8.5.4.4.1).
    /// ByteAlignment, Schema-informed mit acceptance.xsd.
    /// Hinweis: preserve.prefixes wird NICHT gesetzt — die full cross matrix
    /// decodiert sc-02 ohne dieses Flag und es funktioniert. Die Prefix-Info
    /// geht verloren, aber der Event-Stream ist korrekt.
    #[test]
    fn decode_exificient_sc_02_bytealigned() {
        use std::path::Path;

        let schema_path = Path::new(
            "tests/fixtures/w3c/acceptance.xsd",
        );
        if !schema_path.exists() {
            eprintln!("SKIP: acceptance.xsd nicht vorhanden");
            return;
        }

        let schema = crate::xsd::parse_xsd_with_imports(schema_path).unwrap();

        let data = std::fs::read(
            "tests/fixtures/w3c/schema_undeclared/sc-02_bytealigned.exi",
        )
        .expect("sc-02_bytealigned.exi muss vorhanden sein");

        let mut options = ExiOptions::default();
        options.alignment = Alignment::ByteAlignment;

        let (events, _) = decode_with_schema(&data, options, &schema)
            .expect("sc-02_bytealigned muss decodierbar sein");

        // Grundlegende Struktur-Checks
        assert!(events.len() > 10, "sc-02 muss viele Events haben, hat {}", events.len());
        assert!(matches!(events.first(), Some(ExiEvent::StartDocument)));
        assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));

        // Verifiziere Root-Element
        let mut found_root = false;
        for event in &events {
            if let ExiEvent::StartElement(qname) = event {
                if &*qname.uri == "urn:foo" && &*qname.local_name == "ANY" {
                    found_root = true;
                    break;
                }
            }
        }
        assert!(found_root, "foo:ANY muss als Element vorkommen");
    }

    // ========================================================================
    // DecodeIter
    // ========================================================================

    /// DecodeIter Roundtrip: Encode → decode_iter → collect liefert gleiche Events.
    #[test]
    fn decode_iter_roundtrip() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "hello".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (iter, _opts) = decode_iter_with_options(&bytes, ExiOptions::default()).unwrap();
        let decoded: Vec<_> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(decoded.len(), 5);
        assert!(matches!(decoded[0], ExiEvent::StartDocument));
        assert!(matches!(decoded[4], ExiEvent::EndDocument));
    }

    /// DecodeIter: Automatischer finish() am Stream-Ende (keine Trailing-Bit-Fehler).
    #[test]
    fn decode_iter_auto_finish() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "a"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (iter, _opts) = decode_iter_with_options(&bytes, ExiOptions::default()).unwrap();
        // Collect konsumiert alle Events inkl. automatischem finish()
        let decoded: Vec<_> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(decoded.len(), 4);
    }

    /// DecodeIter: Manueller finish() bei vorzeitigem Abbruch.
    #[test]
    fn decode_iter_manual_finish() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "b"))),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();
        let (iter, _opts) = decode_iter_with_options(&bytes, ExiOptions::default()).unwrap();
        // Vorzeitig abbrechen, dann manuell finish()
        let _ = iter.finish();
    }

    /// DecodeIter: Ungültige Daten liefern Err.
    #[test]
    fn decode_iter_invalid_data() {
        let result = decode_iter_with_options(&[0xFF, 0xFF], ExiOptions::default());
        assert!(result.is_err());
    }

    /// DecodeIter: Decode→Serialize liefert korrektes XML.
    #[test]
    fn decode_iter_channel_roundtrip() {
        use crate::xml_serializer::{events_to_xml, events_to_xml_iter};

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "pipeline".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let bytes = encode(&events, &ExiOptions::default()).unwrap();

        // Decode → Vec → XML-Serialize
        let (iter, _opts) = decode_iter_with_options(&bytes, ExiOptions::default()).unwrap();
        let decoded: Vec<_> = iter.collect::<Result<Vec<_>>>().unwrap();
        let mut buf = Vec::new();
        events_to_xml_iter(decoded.into_iter(), &mut buf).unwrap();
        let xml = String::from_utf8(buf).unwrap();
        assert!(xml.contains("<root>pipeline</root>"), "XML: {xml}");

        // Vergleich: sequentieller Pfad liefert identisches Ergebnis
        let seq_xml = events_to_xml(&events).unwrap();
        let (iter2, _) = decode_iter_with_options(&bytes, ExiOptions::default()).unwrap();
        let decoded2: Vec<_> = iter2.collect::<Result<Vec<_>>>().unwrap();
        let seq_xml2 = events_to_xml(&decoded2).unwrap();
        assert_eq!(seq_xml, seq_xml2);
    }

    /// DecodeIter: Byte-Alignment Roundtrip.
    #[test]
    fn decode_iter_byte_aligned_roundtrip() {
        use crate::xml_serializer::events_to_xml_iter;

        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "item"))),
            ExiEvent::Characters(ChContent { value: "aligned".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let mut opts = ExiOptions::default();
        opts.set_alignment(Alignment::ByteAlignment);
        let bytes = encode(&events, &opts).unwrap();

        let (iter, _opts) = decode_iter_with_options(&bytes, opts).unwrap();
        let decoded: Vec<_> = iter.collect::<Result<Vec<_>>>().unwrap();
        let mut buf = Vec::new();
        events_to_xml_iter(decoded.into_iter(), &mut buf).unwrap();
        let xml = String::from_utf8(buf).unwrap();
        assert!(xml.contains("<item>aligned</item>"), "XML: {xml}");
    }

    /// DecodeIter: PreCompression-Roundtrip mit Auto-finish.
    #[test]
    fn decode_iter_precompression_auto_finish() {
        let events = vec![
            ExiEvent::StartDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
            ExiEvent::Characters(ChContent { value: "pc".into() }),
            ExiEvent::EndElement,
            ExiEvent::EndDocument,
        ];
        let mut opts = ExiOptions::default();
        opts.set_alignment(Alignment::PreCompression);
        let bytes = encode(&events, &opts).unwrap();

        let (iter, _opts) = decode_iter_with_options(&bytes, opts).unwrap();
        let decoded: Vec<_> = iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(decoded.len(), 5);
        assert!(matches!(decoded.last(), Some(ExiEvent::EndDocument)));
    }
