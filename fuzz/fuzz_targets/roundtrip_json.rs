#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(json) = std::str::from_utf8(data) {
        if let Ok(exi) = erxi::encode_json(json) {
            let _ = erxi::decode_json(&exi);
        }
    }
});
