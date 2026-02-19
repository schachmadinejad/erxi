use erxi::encoder::{encode, encode_with_schema};
use erxi::options::{Alignment, ExiOptions};
use erxi::parse_xml_events_with_options;
use erxi::schema::SchemaInfo;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::path::Path;

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

    {
        let mut p = *opts.preserve();
        if name.contains("schema_undeclared/cm-") {
            p.comments = true;
            p.pis = true;
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

    // Exificient ignores strict when a preserve option is set (Spec 5.4,
    // spec/exi-spec.txt lines 712-733).
    if opts.strict()
        && (opts.preserve().comments
            || opts.preserve().pis
            || opts.preserve().dtd
            || opts.preserve().prefixes
            || opts.self_contained())
    {
        opts.set_strict(false);
    }

    if name.contains("elementFragment")
        || name.contains("fragment-")
        || name.contains("_fragment")
    {
        opts.set_fragment(true);
    }

    opts
}

fn schema_from_path(path: Option<&str>) -> Result<Option<&'static SchemaInfo>, String> {
    if let Some(p) = path {
        thread_local! {
            static SCHEMAS: RefCell<HashMap<String, &'static SchemaInfo>> = RefCell::new(HashMap::new());
        }
        let cached = SCHEMAS.with(|c| c.borrow().get(p).copied());
        if let Some(schema) = cached {
            return Ok(Some(schema));
        }
        let parsed = parse_xsd_with_imports(Path::new(p))
            .map_err(|e| format!("schema parse failed: {e}"))?;
        let leaked = Box::leak(Box::new(parsed));
        SCHEMAS.with(|c| c.borrow_mut().insert(p.to_string(), leaked));
        Ok(Some(leaked))
    } else {
        Ok(None)
    }
}

fn main() {
    let mut args = env::args().skip(1);
    let xml = args.next().expect("usage: xml_to_exi <xml> <out.exi> [--schema <path>]");
    let out = args.next().expect("usage: xml_to_exi <xml> <out.exi> [--schema <path>]");
    let mut schema_path: Option<String> = None;
    while let Some(arg) = args.next() {
        if arg == "--schema" {
            schema_path = args.next();
        }
    }

    let out_path = Path::new(&out);
    let opts = options_from_filename(out_path);

    let events = parse_xml_events_with_options(Path::new(&xml), &opts)
        .expect("XML parse failed");

    let schema = schema_from_path(schema_path.as_deref()).expect("schema parse failed");

    let bytes = if let Some(schema) = schema {
        encode_with_schema(&events, &opts, schema).expect("encode_with_schema failed")
    } else {
        encode(&events, &opts).expect("encode failed")
    };

    std::fs::write(out_path, &bytes).expect("write EXI failed");
}
