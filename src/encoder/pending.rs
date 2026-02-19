/// Kompakter Eintrag für einen gepufferten Value (Spec 9).
///
/// Statt `(u16, Rc<str>, Option<Rc<TypeDefinition>>)` (32 Bytes + Heap)
/// speichern wir nur Indizes (16 Bytes, kein Heap):
/// - String-Bytes liegen im gemeinsamen `pending_string_buffer`
/// - TypeDefs sind in `pending_type_defs` dedupliziert (~10 Einträge)
pub(super) struct PendingValue {
    /// Byte-Offset im `pending_string_buffer`.
    /// `usize` statt `u32`, damit Dateien >4 GB korrekt behandelt werden.
    pub(super) str_offset: usize,
    /// Byte-Länge des Value-Strings.
    pub(super) str_len: u32,
    /// Channel-Index (verweist auf `pending_channel_qnames`).
    pub(super) channel_idx: u16,
    /// Type-Index: 0 = None, 1..=N = pending_type_defs[idx-1].
    pub(super) type_idx: u16,
}
