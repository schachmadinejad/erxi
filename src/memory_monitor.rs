//! RAM-Monitoring fuer String Table bei grossen Dateien.
//!
//! Ueberwacht den RSS-Verbrauch via `/proc/self/statm` und warnt bei
//! Schwellwerten (50%, 75%, 90%). Bei >=95% wird mit `MemoryLimitExceeded`
//! abgebrochen, bevor Linux durch Swap-Thrashing unbrauchbar wird.

use crate::{Error, Result};
use log::warn;

/// RAM-Monitor fuer String Table Wachstum.
///
/// Prueft periodisch den RSS-Verbrauch und warnt bei Schwellwerten.
/// Bei >=95% wird ein harter Abbruch ausgeloest.
#[derive(Clone)]
pub struct MemoryMonitor {
    /// Totaler physischer RAM in Bytes (aus /proc/meminfo).
    total_ram: u64,
    /// Page-Size in Bytes (via sysconf).
    page_size: u64,
    /// Schwellwerte in Bytes: 50%, 75%, 90%, 95%.
    thresholds: [u64; 4],
    /// Bitmaske: welche Warnungen bereits ausgegeben wurden (4 Bits).
    warnings_emitted: u8,
    /// Geschaetzte Bytes seit letztem Check.
    bytes_since_check: u64,
    /// Intervall fuer RSS-Checks (adaptiv nach RSS).
    check_interval_bytes: u64,
}

/// Normales Check-Intervall in Bytes (10 MiB).
const CHECK_INTERVAL_BYTES_NORMAL: u64 = 10 * 1024 * 1024;

/// Erhoehtes Check-Intervall in Bytes wenn RSS >= 80% (1 MiB).
const CHECK_INTERVAL_BYTES_CRITICAL: u64 = 1 * 1024 * 1024;

impl MemoryMonitor {
    /// Erstellt einen neuen MemoryMonitor.
    ///
    /// Liest `/proc/meminfo` fuer den totalen RAM. Gibt `None` zurueck
    /// auf nicht-Linux-Systemen oder wenn `/proc/meminfo` nicht lesbar ist.
    pub fn new() -> Option<Self> {
        let total_ram = read_effective_ram_limit()?;
        if total_ram == 0 {
            return None;
        }
        let page_size = page_size_bytes();

        let thresholds = [
            total_ram * 50 / 100,
            total_ram * 75 / 100,
            total_ram * 90 / 100,
            total_ram * 95 / 100,
        ];

        Some(Self {
            total_ram,
            page_size,
            thresholds,
            warnings_emitted: 0,
            bytes_since_check: 0,
            check_interval_bytes: CHECK_INTERVAL_BYTES_NORMAL,
        })
    }

    /// Erstellt einen Monitor der beim naechsten Check sofort MemoryLimitExceeded zurueckgibt.
    /// Nur fuer Tests — simuliert 95% RAM ohne echte /proc-Abfrage.
    #[cfg(test)]
    pub(crate) fn always_fail() -> Self {
        Self {
            total_ram: 8_000_000_000,
            page_size: 4096,
            thresholds: [0, 0, 0, 0], // 95%-Schwelle = 0 → immer ueberschritten
            warnings_emitted: 0b1111,  // Warnungen bereits "ausgegeben"
            bytes_since_check: 0,
            check_interval_bytes: 1,   // Sofort beim 1. Eintrag pruefen
        }
    }

    /// Wird bei jedem neuen String-Table-Eintrag aufgerufen.
    ///
    /// Prueft periodisch den RSS-Verbrauch und gibt Warnungen aus.
    /// Bei >=95% wird `Err(MemoryLimitExceeded)` zurueckgegeben.
    #[inline]
    pub fn on_entry_added(&mut self, estimated_bytes: u64) -> Result<()> {
        self.bytes_since_check = self.bytes_since_check.saturating_add(estimated_bytes.max(1));
        if self.bytes_since_check < self.check_interval_bytes {
            return Ok(());
        }
        self.check_rss()
    }

    /// Fuehrt den eigentlichen RSS-Check durch (Cold Path).
    #[cold]
    fn check_rss(&mut self) -> Result<()> {
        self.bytes_since_check = 0;

        let Some(rss_bytes) = read_rss_bytes(self.page_size) else {
            return Ok(());
        };

        // Adaptives Intervall: >= 80% → haeufiger pruefen
        self.check_interval_bytes = if rss_bytes >= self.total_ram * 80 / 100 {
            CHECK_INTERVAL_BYTES_CRITICAL
        } else {
            CHECK_INTERVAL_BYTES_NORMAL
        };

        // 95% → harter Abbruch
        if rss_bytes >= self.thresholds[3] {
            return Err(Error::MemoryLimitExceeded {
                rss_bytes,
                total_bytes: self.total_ram,
            });
        }

        let total_gb = bytes_to_gb(self.total_ram);
        let rss_gb = bytes_to_gb(rss_bytes);

        // 90% Warnung
        if rss_bytes >= self.thresholds[2] && (self.warnings_emitted & 0b0100) == 0 {
            self.warnings_emitted |= 0b0100;
            warn!(
                "[erxi] Warnung: 90% RAM belegt ({rss_gb:.1}/{total_gb:.1} GB). Abbruch bei 95%."
            );
        }

        // 75% Warnung
        if rss_bytes >= self.thresholds[1] && (self.warnings_emitted & 0b0010) == 0 {
            self.warnings_emitted |= 0b0010;
            warn!(
                "[erxi] Warnung: 75% RAM belegt ({rss_gb:.1}/{total_gb:.1} GB). \
                 Tipp: --value-capacity oder --value-max-length setzen."
            );
        }

        // 50% Warnung
        if rss_bytes >= self.thresholds[0] && (self.warnings_emitted & 0b0001) == 0 {
            self.warnings_emitted |= 0b0001;
            warn!(
                "[erxi] Warnung: 50% RAM belegt ({rss_gb:.1}/{total_gb:.1} GB). \
                 String Table waechst."
            );
        }

        Ok(())
    }
}

/// Konvertiert Bytes in Gigabyte (f64).
fn bytes_to_gb(bytes: u64) -> f64 {
    bytes as f64 / (1024.0 * 1024.0 * 1024.0)
}

/// Liest MemTotal aus `/proc/meminfo` (Bytes).
fn read_total_ram() -> Option<u64> {
    let content = std::fs::read_to_string("/proc/meminfo").ok()?;
    for line in content.lines() {
        if let Some(rest) = line.strip_prefix("MemTotal:") {
            let rest = rest.trim();
            let kb_str = rest.strip_suffix("kB").unwrap_or(rest).trim();
            let kb: u64 = kb_str.parse().ok()?;
            return Some(kb * 1024);
        }
    }
    None
}

/// Liest das cgroup Memory-Limit (Bytes), falls gesetzt.
fn read_cgroup_limit_bytes() -> Option<u64> {
    // cgroup v2
    if let Ok(content) = std::fs::read_to_string("/sys/fs/cgroup/memory.max") {
        let s = content.trim();
        if s != "max" {
            if let Ok(val) = s.parse::<u64>() {
                if val > 0 {
                    return Some(val);
                }
            }
        }
    }

    // cgroup v1
    if let Ok(content) = std::fs::read_to_string("/sys/fs/cgroup/memory/memory.limit_in_bytes") {
        let s = content.trim();
        if let Ok(val) = s.parse::<u64>() {
            // Sehr große Werte bedeuten oft "unlimited"
            if val > 0 && val < (1u64 << 60) {
                return Some(val);
            }
        }
    }

    None
}

/// Liest das effektive RAM-Limit (Bytes): min(MemTotal, cgroup-limit).
fn read_effective_ram_limit() -> Option<u64> {
    let total = read_total_ram()?;
    let limit = read_cgroup_limit_bytes();
    Some(limit.map_or(total, |l| total.min(l)))
}

/// Liest RSS aus `/proc/self/statm` (Bytes).
///
/// statm Format: size resident shared text lib data dt
/// Feld 1 (resident) × Page-Size = RSS in Bytes.
fn read_rss_bytes(page_size: u64) -> Option<u64> {
    let content = std::fs::read_to_string("/proc/self/statm").ok()?;
    let rss_pages: u64 = content.split_whitespace().nth(1)?.parse().ok()?;
    Some(rss_pages * page_size)
}

/// Bestimmt die Page-Size (Bytes).
fn page_size_bytes() -> u64 {
    #[cfg(unix)]
    {
        let ps = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        if ps > 0 {
            return ps as u64;
        }
    }
    4096
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_total_ram_returns_value_on_linux() {
        // Auf Linux sollte /proc/meminfo lesbar sein
        if std::path::Path::new("/proc/meminfo").exists() {
            let total = read_total_ram();
            assert!(total.is_some(), "MemTotal nicht lesbar");
            assert!(total.unwrap() > 0, "MemTotal == 0");
        }
    }

    #[test]
    fn read_rss_bytes_returns_value_on_linux() {
        if std::path::Path::new("/proc/self/statm").exists() {
            let rss = read_rss_bytes(page_size_bytes());
            assert!(rss.is_some(), "RSS nicht lesbar");
            assert!(rss.unwrap() > 0, "RSS == 0");
        }
    }

    #[test]
    fn new_returns_some_on_linux() {
        if std::path::Path::new("/proc/meminfo").exists() {
            let monitor = MemoryMonitor::new();
            assert!(monitor.is_some());
        }
    }

    #[test]
    fn on_entry_added_below_interval_is_noop() {
        if let Some(mut monitor) = MemoryMonitor::new() {
            // Unter dem Check-Intervall sollte kein Fehler auftreten
            for _ in 0..100 {
                assert!(monitor.on_entry_added(1).is_ok());
            }
        }
    }

    #[test]
    fn check_interval_adapts() {
        if let Some(mut monitor) = MemoryMonitor::new() {
            // Erzwinge einen Check
            monitor.bytes_since_check = monitor.check_interval_bytes;
            let _ = monitor.check_rss();
            // Intervall sollte gesetzt sein (entweder normal oder critical)
            assert!(
                monitor.check_interval_bytes == CHECK_INTERVAL_BYTES_NORMAL
                    || monitor.check_interval_bytes == CHECK_INTERVAL_BYTES_CRITICAL
            );
        }
    }

    #[test]
    fn memory_limit_exceeded_error_format() {
        let err = Error::MemoryLimitExceeded {
            rss_bytes: 7_600_000_000,
            total_bytes: 8_000_000_000,
        };
        let msg = err.to_string();
        assert!(msg.contains("95%"), "{msg}");
        assert!(msg.contains("7.1"), "{msg}");
        assert!(msg.contains("7.5"), "{msg}");
    }

    #[test]
    fn always_fail_triggers_on_first_entry() {
        // always_fail() Monitor loest sofort MemoryLimitExceeded aus
        if !std::path::Path::new("/proc/self/statm").exists() {
            return; // Nur auf Linux testbar
        }
        let mut monitor = MemoryMonitor::always_fail();
        let result = monitor.on_entry_added(1);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            matches!(err, Error::MemoryLimitExceeded { .. }),
            "Erwartete MemoryLimitExceeded, bekam: {err}"
        );
    }

    #[test]
    fn propagation_string_table_encode_value() {
        // MemoryLimitExceeded propagiert durch StringTable::encode_value
        if !std::path::Path::new("/proc/self/statm").exists() {
            return;
        }
        use crate::string_table::StringTable;
        use crate::qname::QName;

        let mut st = StringTable::new();
        st.set_memory_monitor(MemoryMonitor::always_fail());

        let qname = QName::new("http://example.org", "elem");
        let result = st.encode_value(&qname, "test-value");
        assert!(result.is_err(), "encode_value sollte MemoryLimitExceeded propagieren");
        assert!(
            matches!(result.unwrap_err(), Error::MemoryLimitExceeded { .. }),
        );
    }

    #[test]
    fn propagation_string_table_decode_value_miss() {
        // MemoryLimitExceeded propagiert durch StringTable::decode_value_miss
        if !std::path::Path::new("/proc/self/statm").exists() {
            return;
        }
        use crate::string_table::StringTable;
        use crate::qname::QName;

        let mut st = StringTable::new();
        st.set_memory_monitor(MemoryMonitor::always_fail());

        let qname = QName::new("http://example.org", "elem");
        let result = st.decode_value_miss(&qname, "test-value");
        assert!(result.is_err(), "decode_value_miss sollte MemoryLimitExceeded propagieren");
        assert!(
            matches!(result.unwrap_err(), Error::MemoryLimitExceeded { .. }),
        );
    }

    #[test]
    fn propagation_encoder_encode_event() {
        // MemoryLimitExceeded propagiert durch den gesamten Encoder-Stack
        if !std::path::Path::new("/proc/self/statm").exists() {
            return;
        }
        use std::rc::Rc;
        use crate::encoder::Encoder;
        use crate::event::{ExiEvent, ChContent};
        use crate::qname::QName;

        let mut encoder = Encoder::new(Default::default(), Default::default()).unwrap();
        encoder.set_memory_monitor(MemoryMonitor::always_fail());

        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(Rc::new(QName::new("", "root")))).unwrap();

        // Characters-Event triggert String Table Miss → MemoryLimitExceeded
        let ch = ExiEvent::Characters(ChContent { value: "test-value".into() });
        let result = encoder.encode_event(&ch);
        assert!(result.is_err(), "Encoder::encode_event sollte MemoryLimitExceeded propagieren");
        assert!(
            matches!(result.unwrap_err(), Error::MemoryLimitExceeded { .. }),
            "Erwartete MemoryLimitExceeded"
        );
    }
}
