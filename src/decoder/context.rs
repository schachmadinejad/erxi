use std::collections::VecDeque;
use std::rc::Rc;

use crate::grammar::{GrammarKey, GrammarSystem, NonTerminalId};
use crate::qname::{ExpandedNameId, QName};
use crate::schema::TypeDefinition;
use crate::string_table::StringTable;
use crate::FastHashMap;

/// Kontext pro Element (für Element-Stack).
#[derive(Debug, Clone)]
pub(super) struct ElementContext {
    /// Interned QName-Identität (Copy, für Grammar-Key Lookups).
    pub(super) expanded_name: ExpandedNameId,
    /// Element-Typ aus dem Schema (falls verfügbar).
    pub(super) element_type: Option<Rc<TypeDefinition>>,
    /// Ob das Element nillable ist (Schema-Declaration, falls verfügbar).
    pub(super) is_nillable: bool,
    /// Ob xsi:type im strict Mode erlaubt ist (has_named_sub_types oder union).
    pub(super) needs_xsi_type: bool,
    /// Deferred xsi:type grammar switch (apply after current event completes).
    pub(super) pending_type_switch: Option<Rc<TypeDefinition>>,
    /// Key für die Element-Grammar (global oder lokal).
    pub(super) grammar_key: GrammarKey,
    /// Aktuelles NonTerminal in der Element-Grammar.
    pub(super) current_nt: NonTerminalId,
    /// Ob bereits Content-Events (SE/CH) für dieses Element gesehen wurden.
    pub(super) in_content: bool,
    /// Decode-only: State für xs:all.
    pub(super) all_group: Option<AllGroupState>,
}

/// Leichter Snapshot der Copy-fähigen Felder eines ElementContext.
/// Vermeidet den teuren `.clone()` des gesamten ElementContext
/// (AllGroupState 2x Vec) im Hot-Path von `decode_element_event`.
#[derive(Clone, Copy)]
pub(super) struct ElementSnapshot {
    pub(super) grammar_key: GrammarKey,
    pub(super) current_nt: NonTerminalId,
    pub(super) in_content: bool,
    pub(super) needs_xsi_type: bool,
    pub(super) is_nillable: bool,
}

impl ElementContext {
    pub(super) fn snapshot(&self) -> ElementSnapshot {
        ElementSnapshot {
            grammar_key: self.grammar_key,
            current_nt: self.current_nt,
            in_content: self.in_content,
            needs_xsi_type: self.needs_xsi_type,
            is_nillable: self.is_nillable,
        }
    }
}

/// Decode-only State für xs:all.
///
/// Verwendet `ExpandedNameId` (Copy, 8 Bytes) statt `QName` (3× Arc) —
/// eliminiert Heap-Allokationen bei `mark_seen`.
#[derive(Debug, Clone)]
pub(super) struct AllGroupState {
    pub(super) members: Vec<ExpandedNameId>,
    pub(super) seen: Vec<ExpandedNameId>,
}

impl AllGroupState {
    pub(super) fn is_member(&self, expanded: ExpandedNameId) -> bool {
        self.members.contains(&expanded)
    }

    pub(super) fn is_seen(&self, expanded: ExpandedNameId) -> bool {
        self.seen.contains(&expanded)
    }

    pub(super) fn mark_seen(&mut self, expanded: ExpandedNameId) {
        if self.is_member(expanded) && !self.is_seen(expanded) {
            self.seen.push(expanded);
        }
    }
}

/// Gespeicherter Decoder-State für Self-Contained Elements (Spec 8.5.4.4.1).
#[derive(Clone)]
pub(super) struct ScDecoderState {
    pub(super) string_table: StringTable,
    pub(super) element_grammars: FastHashMap<GrammarKey, Rc<GrammarSystem>>,
    pub(super) document_grammar: Rc<GrammarSystem>,
    pub(super) document_nt: NonTerminalId,
    pub(super) element_stack: Vec<ElementContext>,
    pub(super) next_local_grammar_id: u64,
    pub(super) current_element_type: Option<Rc<TypeDefinition>>,
    pub(super) xsi_nil_active: bool,
    pub(super) xsi_type_seen: bool,
    pub(super) xsi_type_qname: Option<QName>,
    pub(super) saw_invalid_xsi_nil: bool,
    pub(super) xsi_type_skip_learning_hit: bool,
    pub(super) document_in_content: bool,
    pub(super) event_buffer: VecDeque<crate::event::ExiEvent>,
    pub(super) finished: bool,
}
