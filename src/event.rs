//! EXI Event Model (Spec Section 4, Table 4-1, Table 4-2).
//!
//! Defines the 12 EXI event types and their content structures.

use std::rc::Rc;

use crate::qname::QName;

/// Content for Namespace Declaration (NS) events.
///
/// Spec 4, Table 4-1: NS events associate a prefix with a URI or rescind such associations.
///
/// **Constraint (Spec 4):** When `local_element_ns` is true, the `uri` MUST match
/// the URI of the associated SE event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NsContent {
    /// The namespace URI being declared.
    pub uri: Rc<str>,
    /// The prefix bound to this URI (empty string for default namespace).
    pub prefix: Rc<str>,
    /// True if this NS event specifies the namespace of the associated element.
    /// Spec 4: "At most one NS event with local_element_ns=true per element."
    pub local_element_ns: bool,
}

/// Content for Attribute (AT) events.
///
/// Spec 4, Table 4-1: qname + value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtContent {
    /// The qualified name of the attribute.
    pub qname: Rc<QName>,
    /// The attribute value. Currently always String; schema-typed values
    /// (Spec Table 4-2) require generic typing when schema support is added.
    pub value: Rc<str>,
}

/// Content for Characters (CH) events.
///
/// Spec 4, Table 4-1: value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChContent {
    /// The character data. Currently always String; schema-typed values
    /// (Spec Table 4-2) require generic typing when schema support is added.
    pub value: Rc<str>,
}

/// Content for Comment (CM) events.
///
/// Spec 4, Table 4-1: text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CmContent {
    /// The comment text.
    pub text: Rc<str>,
}

/// Content for Processing Instruction (PI) events.
///
/// Spec 4, Table 4-1: name + text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PiContent {
    /// The PI target name.
    pub name: Rc<str>,
    /// The PI data.
    pub text: Rc<str>,
}

/// Content for DOCTYPE (DT) events.
///
/// Spec 4, Table 4-1: name + public + system + text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DtContent {
    /// The document type name.
    pub name: Rc<str>,
    /// The public identifier (empty if none).
    pub public: Rc<str>,
    /// The system identifier (empty if none).
    pub system: Rc<str>,
    /// The internal subset text (empty if none).
    pub text: Rc<str>,
}

/// Content for Entity Reference (ER) events.
///
/// Spec 4, Table 4-1: name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErContent {
    /// The entity name.
    pub name: Rc<str>,
}

impl Default for NsContent {
    fn default() -> Self {
        Self { uri: Rc::from(""), prefix: Rc::from(""), local_element_ns: false }
    }
}

impl Default for CmContent {
    fn default() -> Self {
        Self { text: Rc::from("") }
    }
}

impl Default for PiContent {
    fn default() -> Self {
        Self { name: Rc::from(""), text: Rc::from("") }
    }
}

impl Default for DtContent {
    fn default() -> Self {
        Self {
            name: Rc::from(""),
            public: Rc::from(""),
            system: Rc::from(""),
            text: Rc::from(""),
        }
    }
}

impl Default for ErContent {
    fn default() -> Self {
        Self { name: Rc::from("") }
    }
}

/// EXI Event types as defined in Spec Section 4, Table 4-1.
///
/// Each variant represents one of the 12 EXI event types.
/// See Appendix B for mapping to XML Information Items.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExiEvent {
    /// Start Document - marks the beginning of an EXI document.
    StartDocument,
    /// End Document - marks the end of an EXI document.
    EndDocument,
    /// Start Element - begins an element with the given qname.
    StartElement(Rc<QName>),
    /// End Element - closes the current element.
    EndElement,
    /// Attribute - an attribute with qname and value.
    Attribute(AtContent),
    /// Characters - character data content.
    Characters(ChContent),
    /// Namespace Declaration - binds a prefix to a URI.
    NamespaceDeclaration(NsContent),
    /// Comment - an XML comment.
    Comment(CmContent),
    /// Processing Instruction - an XML PI.
    ProcessingInstruction(PiContent),
    /// DOCTYPE - document type declaration.
    DocType(DtContent),
    /// Entity Reference - an unexpanded entity reference.
    EntityReference(ErContent),
    /// Self Contained - marks a self-contained element for random access.
    /// Spec 4: "An SE event may be followed by a SC event, indicating the element
    /// is self-contained and can be read independently from the rest of the EXI body."
    SelfContained,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Hilfsfunktion: QName ohne Namespace erstellen.
    fn qname_local(local_name: &str) -> Rc<QName> {
        Rc::new(QName::new("", local_name))
    }

    /// Hilfsfunktion: QName mit Namespace erstellen.
    fn qname_ns(uri: &str, local_name: &str, prefix: Option<&str>) -> Rc<QName> {
        Rc::new(match prefix {
            Some(pfx) => QName::with_prefix(uri, local_name, pfx),
            None => QName::new(uri, local_name),
        })
    }

    // ==================== Konstruktion Tests ====================

    /// Spec 4, Table 4-1: SD event has no content.
    #[test]
    fn start_document_konstruktion() {
        assert!(matches!(ExiEvent::StartDocument, ExiEvent::StartDocument));
    }

    /// Spec 4, Table 4-1: ED event has no content.
    #[test]
    fn end_document_konstruktion() {
        assert!(matches!(ExiEvent::EndDocument, ExiEvent::EndDocument));
    }

    /// Spec 4, Table 4-1: SE event contains qname.
    #[test]
    fn start_element_konstruktion() {
        let qname = qname_ns("http://example.org", "element", Some("ex"));
        let ExiEvent::StartElement(q) = ExiEvent::StartElement(qname.clone()) else {
            panic!("Expected StartElement");
        };

        assert_eq!(&*q.uri, "http://example.org");
        assert_eq!(&*q.local_name, "element");
        assert_eq!(q.prefix.as_deref(), Some("ex"));
    }

    /// Spec 4, Table 4-1: EE event has no content.
    #[test]
    fn end_element_konstruktion() {
        assert!(matches!(ExiEvent::EndElement, ExiEvent::EndElement));
    }

    /// Spec 4, Table 4-1: AT event contains qname + value.
    #[test]
    fn attribute_konstruktion() {
        let content = AtContent {
            qname: qname_local("id"),
            value: "123".into(),
        };
        let ExiEvent::Attribute(at) = ExiEvent::Attribute(content) else {
            panic!("Expected Attribute");
        };

        assert_eq!(&*at.qname.local_name, "id");
        assert_eq!(&*at.value, "123");
    }

    /// Spec 4, Table 4-1: CH event contains value.
    #[test]
    fn characters_konstruktion() {
        let content = ChContent {
            value: "Hello, World!".into(),
        };
        let ExiEvent::Characters(ch) = ExiEvent::Characters(content) else {
            panic!("Expected Characters");
        };

        assert_eq!(&*ch.value, "Hello, World!");
    }

    /// Spec 4, Table 4-1: NS event contains uri + prefix + local-element-ns.
    #[test]
    fn namespace_declaration_konstruktion() {
        let content = NsContent {
            uri: "http://www.w3.org/1999/xhtml".into(),
            prefix: "html".into(),
            local_element_ns: false,
        };
        let ExiEvent::NamespaceDeclaration(ns) = ExiEvent::NamespaceDeclaration(content) else {
            panic!("Expected NamespaceDeclaration");
        };

        assert_eq!(&*ns.uri, "http://www.w3.org/1999/xhtml");
        assert_eq!(&*ns.prefix, "html");
        assert!(!ns.local_element_ns);
    }

    /// Spec 4, Table 4-1: CM event contains text.
    #[test]
    fn comment_konstruktion() {
        let content = CmContent {
            text: "This is a comment".into(),
        };
        let ExiEvent::Comment(cm) = ExiEvent::Comment(content) else {
            panic!("Expected Comment");
        };

        assert_eq!(&*cm.text, "This is a comment");
    }

    /// Spec 4, Table 4-1: PI event contains name + text.
    #[test]
    fn processing_instruction_konstruktion() {
        let content = PiContent {
            name: "xml-stylesheet".into(),
            text: "type=\"text/xsl\" href=\"style.xsl\"".into(),
        };
        let ExiEvent::ProcessingInstruction(pi) = ExiEvent::ProcessingInstruction(content) else {
            panic!("Expected ProcessingInstruction");
        };

        assert_eq!(&*pi.name, "xml-stylesheet");
        assert_eq!(&*pi.text, "type=\"text/xsl\" href=\"style.xsl\"");
    }

    /// Spec 4, Table 4-1: DT event contains name + public + system + text.
    #[test]
    fn doctype_konstruktion() {
        let content = DtContent {
            name: "html".into(),
            public: "-//W3C//DTD XHTML 1.0 Strict//EN".into(),
            system: "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd".into(),
            text: "".into(),
        };
        let ExiEvent::DocType(dt) = ExiEvent::DocType(content) else {
            panic!("Expected DocType");
        };

        assert_eq!(&*dt.name, "html");
        assert_eq!(&*dt.public, "-//W3C//DTD XHTML 1.0 Strict//EN");
        assert_eq!(
            &*dt.system,
            "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
        );
        assert!(dt.text.is_empty());
    }

    /// Spec 4, Table 4-1: ER event contains name.
    #[test]
    fn entity_reference_konstruktion() {
        let content = ErContent {
            name: "nbsp".into(),
        };
        let ExiEvent::EntityReference(er) = ExiEvent::EntityReference(content) else {
            panic!("Expected EntityReference");
        };

        assert_eq!(&*er.name, "nbsp");
    }

    /// Spec 4, Table 4-1: SC event has no content.
    #[test]
    fn self_contained_konstruktion() {
        assert!(matches!(ExiEvent::SelfContained, ExiEvent::SelfContained));
    }

    // ==================== NS Event Semantik Tests ====================

    /// Spec 4: NS event with empty URI rescinds namespace association (undeclare).
    #[test]
    fn ns_undeclare_namespace() {
        let content = NsContent {
            uri: "".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        };
        let ExiEvent::NamespaceDeclaration(ns) = ExiEvent::NamespaceDeclaration(content) else {
            panic!("Expected NamespaceDeclaration");
        };

        assert!(ns.uri.is_empty(), "Empty URI rescinds the prefix binding");
        assert_eq!(&*ns.prefix, "ex");
    }

    /// Spec 4: NS event with empty prefix declares default namespace.
    #[test]
    fn ns_default_namespace() {
        let content = NsContent {
            uri: "http://example.org".into(),
            prefix: "".into(),
            local_element_ns: false,
        };
        let ExiEvent::NamespaceDeclaration(ns) = ExiEvent::NamespaceDeclaration(content) else {
            panic!("Expected NamespaceDeclaration");
        };

        assert!(ns.prefix.is_empty(), "Empty prefix = default namespace");
        assert_eq!(&*ns.uri, "http://example.org");
    }

    /// Spec 4: local_element_ns=true indicates this NS specifies the element's namespace.
    #[test]
    fn ns_local_element_ns_flag() {
        let content = NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: true,
        };

        assert!(
            content.local_element_ns,
            "Flag indicates element namespace source"
        );
    }

    // ==================== Clone und Eq Tests ====================

    /// Alle Event-Typen müssen Clone implementieren.
    #[test]
    fn events_are_clone() {
        let events = [
            ExiEvent::StartDocument,
            ExiEvent::EndDocument,
            ExiEvent::StartElement(qname_local("test")),
            ExiEvent::EndElement,
            ExiEvent::Attribute(AtContent {
                qname: qname_local("attr"),
                value: "val".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "text".into(),
            }),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "http://example.org".into(),
                prefix: "ex".into(),
                local_element_ns: false,
            }),
            ExiEvent::Comment(CmContent {
                text: "comment".into(),
            }),
            ExiEvent::ProcessingInstruction(PiContent {
                name: "pi".into(),
                text: "data".into(),
            }),
            ExiEvent::DocType(DtContent {
                name: "html".into(),
                public: "".into(),
                system: "".into(),
                text: "".into(),
            }),
            ExiEvent::EntityReference(ErContent {
                name: "amp".into(),
            }),
            ExiEvent::SelfContained,
        ];

        for event in &events {
            assert_eq!(event, &event.clone());
        }
    }

    /// Content-Structs müssen PartialEq implementieren.
    #[test]
    fn content_structs_equality() {
        let ns1 = NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: true,
        };
        let ns2 = ns1.clone();
        let ns3 = NsContent {
            uri: "http://other.org".into(),
            prefix: "ex".into(),
            local_element_ns: true,
        };

        assert_eq!(ns1, ns2);
        assert_ne!(ns1, ns3);
    }

    /// Debug-Ausgabe für Events.
    #[test]
    fn events_have_debug() {
        let debug = format!("{:?}", ExiEvent::StartDocument);
        assert!(debug.contains("StartDocument"));

        let se = ExiEvent::StartElement(qname_ns("http://example.org", "test", Some("ex")));
        let debug = format!("{:?}", se);
        assert!(debug.contains("StartElement"));
        assert!(debug.contains("example.org"));
    }

    // ==================== Edge Cases ====================

    /// Spec 4: Attribut ohne Namespace (leere URI).
    #[test]
    fn attribute_ohne_namespace() {
        let content = AtContent {
            qname: qname_local("class"),
            value: "container".into(),
        };

        assert!(content.qname.uri.is_empty());
        assert!(content.qname.prefix.is_none());
    }

    /// Spec 4: Attribut mit Namespace.
    #[test]
    fn attribute_mit_namespace() {
        let content = AtContent {
            qname: qname_ns("http://www.w3.org/XML/1998/namespace", "lang", Some("xml")),
            value: "en".into(),
        };

        assert_eq!(&*content.qname.uri, "http://www.w3.org/XML/1998/namespace");
        assert_eq!(content.qname.prefix.as_deref(), Some("xml"));
    }

    /// Characters mit leerem String sind valide.
    #[test]
    fn characters_leerer_string() {
        let content = ChContent {
            value: "".into(),
        };
        let ExiEvent::Characters(ch) = ExiEvent::Characters(content) else {
            panic!("Expected Characters");
        };

        assert!(ch.value.is_empty());
    }

    /// DOCTYPE mit internem Subset.
    #[test]
    fn doctype_mit_internal_subset() {
        let content = DtContent {
            name: "doc".into(),
            public: "".into(),
            system: "".into(),
            text: "<!ENTITY copyright \"(c) 2024\">".into(),
        };

        assert!(!content.text.is_empty());
        assert!(content.text.contains("ENTITY"));
    }

    /// PI ohne Daten (nur Target).
    #[test]
    fn pi_ohne_daten() {
        let content = PiContent {
            name: "xml".into(),
            text: "".into(),
        };

        assert!(!content.name.is_empty());
        assert!(content.text.is_empty());
    }
}
