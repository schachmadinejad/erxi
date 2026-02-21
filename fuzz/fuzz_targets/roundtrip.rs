#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(xml) = std::str::from_utf8(data) {
        let opts = erxi::ExiOptions::default();
        if let Ok(events) = erxi::parse_xml_events_from_str(xml, &opts) {
            if let Ok(exi) = erxi::encoder::encode(&events, &opts) {
                let _ = erxi::decode(&exi);
            }
        }
    }
});
