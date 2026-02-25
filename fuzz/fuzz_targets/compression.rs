#![no_main]
use libfuzzer_sys::fuzz_target;
use erxi::options::{Alignment, ExiOptions};

fuzz_target!(|data: &[u8]| {
    // Roundtrip XML through compression and pre-compression paths
    if let Ok(xml) = std::str::from_utf8(data) {
        let opts_comp = ExiOptions::default().with_compression();
        if let Ok(events) = erxi::parse_xml_events_from_str(xml, &opts_comp) {
            if let Ok(exi) = erxi::encoder::encode(&events, &opts_comp) {
                let _ = erxi::decode(&exi);
            }
        }

        let opts_pre = ExiOptions::default().with_alignment(Alignment::PreCompression);
        if let Ok(events) = erxi::parse_xml_events_from_str(xml, &opts_pre) {
            if let Ok(exi) = erxi::encoder::encode(&events, &opts_pre) {
                let _ = erxi::decode(&exi);
            }
        }
    }
});
