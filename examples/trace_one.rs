use erxi::decoder::Decoder;
use erxi::options::{Alignment, ExiOptions};
use erxi::schema::SchemaInfo;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::path::Path;

fn exi_testsuite_base() -> String {
    std::env::var("EXI_TESTSUITE_DIR")
        .expect("EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)")
}

fn options_from_filename(path: &Path) -> ExiOptions {
    let name = path.to_string_lossy();
    let mut opts = ExiOptions::default();

    if name.contains("_bytealigned") {
        opts.set_alignment(Alignment::ByteAlignment);
    } else if name.contains("_precompression") {
        opts.set_alignment(Alignment::PreCompression);
    } else if name.contains("_compression") {
        opts.set_compression(true);
    }

    if name.contains("_strict") {
        opts.set_strict(true);
    }

    if !opts.strict() {
        let mut p = *opts.preserve();
        if name.contains("schema_undeclared/cm-") {
            p.comments = true;
            if !name.contains("cm-04") {
                p.pis = true;
            }
        }
        if name.contains("schema_undeclared/pi-") {
            p.pis = true;
        }
        if name.contains("schema_undeclared/er-") {
            p.dtd = true;
        }
        if name.contains("schema_undeclared/namespaceDecl-") {
            p.prefixes = true;
        }

        if name.contains("schema_declared/document-01") {
            p.prefixes = true;
        } else if name.contains("schema_declared/document-02")
            || name.contains("schema_declared/document-03")
            || name.contains("schema_declared/document-04")
            || name.contains("schema_declared/document-05")
            || name.contains("schema_declared/document-06")
            || name.contains("schema_declared/document-07")
        {
            p.dtd = true;
            p.pis = true;
            p.comments = true;
        }

        if name.contains("schema_declared/elementFragment-05")
            || name.contains("schema_declared/elementFragment-06")
        {
            p.dtd = true;
            p.pis = true;
            p.comments = true;
        }

        if name.contains("schema_declared/fragment-01") {
            p.dtd = true;
            p.pis = true;
            p.comments = true;
        } else if name.contains("schema_declared/fragment-02") {
            p.dtd = true;
        } else if name.contains("schema_declared/fragment-03")
            || name.contains("schema_declared/fragment-04")
            || name.contains("schema_declared/fragment-05")
            || name.contains("schema_declared/fragment-06")
        {
            p.pis = true;
        }
        opts.set_preserve(p);
    }

    if name.contains("elementFragment")
        || name.contains("fragment-")
        || name.contains("_fragment")
    {
        opts.set_fragment(true);
    }

    opts
}

fn schema_for_fixture(path: &Path) -> Result<&'static SchemaInfo, String> {
    thread_local! {
        static ACCEPTANCE_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DOCUMENT_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static PARTICLE_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static ELEMENT_FRAGMENT_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_A_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_B_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_C_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_D_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_E_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DUPLICATE_TERMINALS_01_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DUPLICATE_TERMINALS_02_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static SUBSTITUTION_GROUP_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    }

    fn get_or_init(
        cell: &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>,
        f: impl FnOnce() -> SchemaInfo,
    ) -> &'static SchemaInfo {
        cell.with(|c| {
            let mut borrow = c.borrow_mut();
            if let Some(s) = *borrow {
                s
            } else {
                let leaked: &'static SchemaInfo = Box::leak(Box::new(f()));
                *borrow = Some(leaked);
                leaked
            }
        })
    }

    let name = path.file_name().unwrap().to_string_lossy();
    let base = format!("{}/data/interop/schemaInformedGrammar", exi_testsuite_base());
    let declared = format!("{base}/declaredProductions");
    let (xsd_path, cell): (String, &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>) = if name.contains("elementFragment") {
        (format!("{declared}/elementFragment.xsd"), &ELEMENT_FRAGMENT_SCHEMA)
    } else if name.contains("document-") {
        (format!("{declared}/document.xsd"), &DOCUMENT_SCHEMA)
    } else if name.contains("duplicateTerminals-01") {
        (format!("{declared}/duplicateTerminals-01.xsd"), &DUPLICATE_TERMINALS_01_SCHEMA)
    } else if name.contains("duplicateTerminals-02") {
        (format!("{declared}/duplicateTerminals-02.xsd"), &DUPLICATE_TERMINALS_02_SCHEMA)
    } else if name.contains("elementTerm-01") {
        (format!("{declared}/substitutionGroup.xsd"), &SUBSTITUTION_GROUP_SCHEMA)
    } else if name.contains("particle-") || name.contains("complexType-21") || name.contains("complexType-23") {
        (format!("{declared}/particle.xsd"), &PARTICLE_SCHEMA)
    } else if name.contains("fragment-01") || name.contains("fragment-02") {
        (format!("{declared}/fragment-a.xsd"), &FRAGMENT_A_SCHEMA)
    } else if name.contains("fragment-03") {
        (format!("{declared}/fragment-b.xsd"), &FRAGMENT_B_SCHEMA)
    } else if name.contains("fragment-04") {
        (format!("{declared}/fragment-c.xsd"), &FRAGMENT_C_SCHEMA)
    } else if name.contains("fragment-05") {
        (format!("{declared}/fragment-d.xsd"), &FRAGMENT_D_SCHEMA)
    } else if name.contains("fragment-06") {
        (format!("{declared}/fragment-e.xsd"), &FRAGMENT_E_SCHEMA)
    } else {
        (format!("{base}/acceptance.xsd"), &ACCEPTANCE_SCHEMA)
    };

    Ok(get_or_init(cell, || {
        parse_xsd_with_imports(Path::new(&xsd_path))
            .unwrap_or_else(|e| panic!("Schema parse error for {xsd_path}: {e}"))
    }))
}

fn main() {
    let path = std::env::args().nth(1).expect("usage: trace_one <exi>");
    let path = Path::new(&path);
    let data = std::fs::read(path).expect("read");
    let options = options_from_filename(path);
    let schema = schema_for_fixture(path).expect("schema");

    let mut decoder = Decoder::with_schema(&data, options, schema.clone()).expect("decoder");
    if std::env::var("ERXI_FORCE_TIER2").is_ok() {
        decoder.set_force_tier2(true);
    }
    let mut i = 0u32;
    loop {
        let remaining = decoder.remaining_bits();
        match decoder.decode_event() {
            Ok(Some(event)) => {
                eprintln!("{:02}: {:?}  (remaining={})", i, event, remaining);
                i += 1;
            }
            Ok(None) => {
                eprintln!("ED (events={}, remaining={})", i, remaining);
                break;
            }
            Err(e) => {
                eprintln!("ERR at event {}: {} (remaining={})", i, e, remaining);
                break;
            }
        }
    }
    match decoder.finish() {
        Ok(()) => println!("finish: OK"),
        Err(e) => println!("finish: ERR {e}"),
    }
}
