//! PreCompression Channel Ordering (Spec 9.3).
//!
//! Dieses Modul enthält die gemeinsame Logik für die Reihenfolge von Value Channels
//! bei PreCompression (und Compression). Wird von Encoder und Decoder verwendet.
//!
//! # Spec-Referenz
//!
//! - Spec 9.2.1: Structure Channel
//! - Spec 9.2.2: Value Channels
//! - Spec 9.3: Compressed Streams (Channel-Reihenfolge)

use std::rc::Rc;

use crate::FastIndexMap;

/// Channel-Key für PreCompression: URI + LocalName.
///
/// Spec 9: Channels werden nach QName SEMANTISCH gruppiert (URI + local-name),
/// nicht nach Prefix. Gleiche (URI, local-name) mit unterschiedlichen Prefixes
/// gehören zum selben Channel.
///
/// Nutzt `Rc<str>` statt `String` — da QName.uri/local_name bereits `Rc<str>`
/// sind, ist die Erstellung per `Rc::clone` kostenlos (nur Refcount-Increment).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChannelKey {
    pub uri: Rc<str>,
    pub local_name: Rc<str>,
}

impl ChannelKey {
    /// Erstellt einen neuen ChannelKey aus Rc<str>-Referenzen.
    pub fn new(uri: Rc<str>, local_name: Rc<str>) -> Self {
        Self { uri, local_name }
    }
}

/// Sortiert Channels nach Spec 9.3.
///
/// # Reihenfolge
///
/// - Bei ≤100 Values insgesamt: Channels in Reihenfolge des ersten Auftretens
/// - Bei >100 Values: Erst kleine Channels (≤100 Values), dann große (>100),
///   jeweils in Reihenfolge des ersten Auftretens
///
/// # Parameter
///
/// - `channel_counts`: Channels mit Anzahl Values (IndexMap behält Insertion-Order)
/// - `total_values`: Gesamtzahl aller Values
///
/// # Spec-Referenz
///
/// - Spec 9.3: "When there are more than 100 values in any given channel,
///   the smaller channels (those with ≤100 values) are ordered first..."
pub fn order_channels(
    channel_counts: &FastIndexMap<ChannelKey, usize>,
    total_values: usize,
) -> Vec<ChannelKey> {
    if total_values <= 100 {
        // ≤100 Values: Channels in Reihenfolge des ersten Auftretens
        channel_counts.keys().cloned().collect()
    } else {
        // >100 Values: Erst kleine Channels, dann große
        let mut small_channels: Vec<ChannelKey> = Vec::new();
        let mut large_channels: Vec<ChannelKey> = Vec::new();

        for (key, &count) in channel_counts {
            if count <= 100 {
                small_channels.push(key.clone());
            } else {
                large_channels.push(key.clone());
            }
        }

        small_channels.extend(large_channels);
        small_channels
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(local: &str) -> ChannelKey {
        ChannelKey::new(Rc::from(""), Rc::from(local))
    }

    /// Spec 9.3: ≤100 Values behält Reihenfolge.
    #[test]
    fn order_channels_small_preserves_order() {
        let counts: FastIndexMap<_, _> =
            [(key("a"), 10), (key("b"), 20), (key("c"), 30)].into_iter().collect();

        let result = order_channels(&counts, 60);
        assert_eq!(result, vec![key("a"), key("b"), key("c")]);
    }

    /// Spec 9.3: >100 Values sortiert kleine vor große.
    #[test]
    fn order_channels_large_sorts_small_first() {
        let counts: FastIndexMap<_, _> =
            [(key("big"), 105), (key("small"), 3)].into_iter().collect();

        let result = order_channels(&counts, 108);

        assert_eq!(result[0], key("small"));
        assert_eq!(result[1], key("big"));
    }

    /// Spec 9.3: Mehrere kleine Channels behalten ihre relative Reihenfolge.
    #[test]
    fn order_channels_multiple_small_preserve_relative_order() {
        let counts: FastIndexMap<_, _> =
            [(key("big"), 150), (key("small1"), 50), (key("small2"), 30)].into_iter().collect();

        let result = order_channels(&counts, 230);

        assert_eq!(result[0], key("small1"));
        assert_eq!(result[1], key("small2"));
        assert_eq!(result[2], key("big"));
    }

    /// Grenzfall: Genau 100 Values (nicht >100).
    #[test]
    fn order_channels_exactly_100_preserves_order() {
        let counts: FastIndexMap<_, _> =
            [(key("a"), 60), (key("b"), 40)].into_iter().collect();

        let result = order_channels(&counts, 100);
        assert_eq!(result, vec![key("a"), key("b")]);
    }
}
