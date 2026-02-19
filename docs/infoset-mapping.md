# Infoset Mapping (Spec Appendix B)

Mapping of the 11 XML Information Set item types to EXI events.

## Overview

| # | Item Type | EXI Event(s) | Preserve Flag | Test |
|---|-----------|--------------|---------------|------|
| B.1 | Document Information Item | SD, ED | -- | `b1_document_round_trip` |
| B.2 | Element Information Items | SE(qname), EE | -- | `b2_element_round_trip` |
| B.3 | Attribute Information Item | AT(qname, value) | -- | `b3_attribute_round_trip` |
| B.4 | Processing Instruction | PI(name, text) | `preserve.pis` | `b4_pi_round_trip` |
| B.5 | Unexpanded Entity Ref | ER(name) | `preserve.dtd` | `b5_entity_reference_round_trip` |
| B.6 | Character Information Item | CH(value) | -- | `b6_character_round_trip` |
| B.7 | Comment Information Item | CM(text) | `preserve.comments` | `b7_comment_round_trip` |
| B.8 | Document Type Declaration | DT(name, public, system, text) | `preserve.dtd` | `b8_doctype_round_trip` |
| B.9 | Unparsed Entity | Part of DT text | `preserve.dtd` | `b9_unparsed_entity_round_trip` |
| B.10 | Notation | Part of DT text | `preserve.dtd` | `b10_notation_round_trip` |
| B.11 | Namespace Information Item | NS(uri, prefix, local_element_ns) | `preserve.prefixes` | `b11_namespace_round_trip` |

## Property Preservation

### B.1 Document Information Item

| Property | EXI Mapping |
|----------|-------------|
| [children] | CM* PI* DT? SE EE (between SD and ED) |
| [document element] | SE, EE |
| [notations] | Derived from DT text |
| [unparsed entities] | Derived from DT text |
| [base URI] | Base URI of the EXI stream |
| [character encoding scheme] | N/A (EXI-agnostic) |
| [standalone] | Not available |
| [version] | Not available |
| [all declarations processed] | true if all DT declarations were processed |

### B.2 Element Information Items

| Property | EXI Mapping |
|----------|-------------|
| [namespace name] | SE |
| [local name] | SE |
| [prefix] | SE (only with `preserve.prefixes`) |
| [children] | SE* EE* PI* CM* CH* ER* (between SE and EE) |
| [attributes] | AT* |
| [namespace attributes] | NS* |
| [in-scope namespaces] | Derived from NS events of ancestors |
| [base URI] | Base URI of the element |
| [parent] | Derived: last SE without matching EE, or SD |

### B.3 Attribute Information Item

| Property | EXI Mapping |
|----------|-------------|
| [namespace name] | AT |
| [local name] | AT |
| [prefix] | AT (only with `preserve.prefixes`) |
| [normalized value] | AT value |
| [specified] | true if present as AT |
| [attribute type] | Derived from AT and DT |
| [references] | Derived from type and AT value |
| [owner element] | Derived: last SE without matching EE |

### B.4 Processing Instruction Information Item

| Property | EXI Mapping |
|----------|-------------|
| [target] | PI name |
| [content] | PI text |
| [base URI] | Base URI of the PI |
| [notation] | Derived from DTD subset |
| [parent] | Derived: last SE without matching EE |

### B.5 Unexpanded Entity Reference Information Item

| Property | EXI Mapping |
|----------|-------------|
| [name] | ER name |
| [system identifier] | Derived from DTD subset |
| [public identifier] | Derived from DTD subset |
| [declaration base URI] | Base URI of the entity reference |
| [parent] | Derived: last SE without matching EE |

Note: Internal entities are expanded by the XML parser and appear as CH events.
External entities (SYSTEM) are dropped (similar to SAX skippedEntity).
ER events are only produced for custom entity references that are not resolved.

### B.6 Character Information Item

| Property | EXI Mapping |
|----------|-------------|
| [character code] | Individual characters in CH value |
| [element content whitespace] | Derived from [parent] and DT |
| [parent] | Derived: last SE without matching EE |

### B.7 Comment Information Item

| Property | EXI Mapping |
|----------|-------------|
| [content] | CM text |
| [parent] | Derived: last SE without matching EE, or SD |

### B.8 Document Type Declaration Information Item

| Property | EXI Mapping |
|----------|-------------|
| [system identifier] | DT system |
| [public identifier] | DT public |
| [children] | Derived from DT text |
| [parent] | Derived: SD |

### B.9 Unparsed Entity Information Item

| Property | EXI Mapping |
|----------|-------------|
| [name] | Derived from DT text |
| [system identifier] | Derived from DT text |
| [public identifier] | Derived from DT text |
| [declaration base URI] | Base URI of the item |
| [notation name] | Derived from DT text |
| [notation] | Derived from DT text |

### B.10 Notation Information Item

| Property | EXI Mapping |
|----------|-------------|
| [name] | Derived from DT text |
| [system identifier] | Derived from DT text |
| [public identifier] | Derived from DT text |
| [declaration base URI] | Base URI of the item |

### B.11 Namespace Information Item

| Property | EXI Mapping |
|----------|-------------|
| [prefix] | NS prefix |
| [namespace name] | NS uri |

## Unavailable Properties

The following XML Information Set properties are not represented by EXI:

- **[character encoding scheme]** -- EXI uses its own encoding (Spec B.1)
- **[standalone]** -- omitted because no external markup declarations are referenced (Spec B.1)
- **[version]** -- XML version is EXI-agnostic (Spec B.1)

## Derived Properties

Many properties are not stored directly in the EXI stream but are derived
at runtime:

- **[parent]** -- from the element stack (last SE without matching EE)
- **[in-scope namespaces]** -- from NS events of the element and its ancestors
- **[attribute type]** -- from AT event and schema/DTD
- **[notations], [unparsed entities]** -- from DT text content
- **[base URI]** -- from the EXI stream context
