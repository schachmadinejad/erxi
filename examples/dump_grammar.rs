use std::env;
use std::path::Path;

use erxi::grammar::{GrammarSystem, NonTerminalId, Terminal};
use erxi::qname::{QName, StringInterner};
use erxi::xsd::parse_xsd_with_imports;
use erxi::options::ExiOptions;

fn exi_testsuite_base() -> String {
    std::env::var("EXI_TESTSUITE_DIR")
        .expect("EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)")
}

fn main() {
    let mut args = env::args().skip(1);
    let schema_path = args.next().unwrap_or_else(|| {
        format!("{}/data/interop/schemaInformedGrammar/acceptance.xsd", exi_testsuite_base())
    });
    let uri = args.next().unwrap_or_else(|| "urn:foo".to_string());
    let local = args.next().unwrap_or_else(|| "I2".to_string());
    let type_uri = args.next();
    let type_local = args.next();

    let schema = parse_xsd_with_imports(Path::new(&schema_path))
        .unwrap_or_else(|e| panic!("parse_xsd_with_imports failed: {e}"));
    let elem_qname = QName::new(uri, local);
    let elem_decl = if let (Some(t_uri), Some(t_local)) = (type_uri, type_local) {
        let type_qname = QName::new(t_uri, t_local);
        let type_def = schema
            .get_type(&type_qname)
            .unwrap_or_else(|| panic!("type not found: {}", type_qname.local_name));
        let decl = erxi::schema::ElementDeclaration::new(std::rc::Rc::new(elem_qname.clone()))
            .with_type(type_def.clone());
        std::rc::Rc::new(decl)
    } else {
        std::rc::Rc::clone(
            schema
                .get_element(&elem_qname)
                .unwrap_or_else(|| panic!("element not found: {}", elem_qname.local_name)),
        )
    };
    let mut opts = ExiOptions::default();
    {
        let mut p = *opts.preserve();
        if std::env::var("ERXI_PRESERVE_COMMENTS").is_ok() {
            p.comments = true;
        }
        if std::env::var("ERXI_PRESERVE_PIS").is_ok() {
            p.pis = true;
        }
        if std::env::var("ERXI_PRESERVE_DTD").is_ok() {
            p.dtd = true;
        }
        opts.set_preserve(p);
    }
    let mut interner = StringInterner::new();
    let grammar = GrammarSystem::schema_informed_element(elem_decl.as_ref(), &opts, &mut interner).unwrap();

    println!("Grammar type: {:?}", grammar.grammar_type());
    println!("Start NT: {:?}", grammar.start());

    for nt in grammar.non_terminals() {
        println!("\n{:?}:", nt.id());
        for prod in nt.productions() {
            let rhs = prod.right_hand_side;
            let ec = prod.event_code.as_ref();
            match &prod.terminal {
                Terminal::Attribute(_) => {
                    println!("  AT -> {:?} (rhs={:?}, ec={:?})", prod.terminal, rhs, ec);
                }
                Terminal::StartElement(_) => {
                    println!("  SE -> {:?} (rhs={:?}, ec={:?})", prod.terminal, rhs, ec);
                }
                Terminal::EndElement => {
                    println!("  EE -> {:?} (rhs={:?}, ec={:?})", prod.terminal, rhs, ec);
                }
                term => {
                    println!("  {:?} (rhs={:?}, ec={:?})", term, rhs, ec);
                }
            }
        }
    }

    if let Some(nt) = grammar.get(NonTerminalId::SchemaType(0)) {
        println!("\nSchemaType(0) terminals:");
        for prod in nt.productions() {
            println!("  {:?}", prod.terminal);
        }
    }
}
