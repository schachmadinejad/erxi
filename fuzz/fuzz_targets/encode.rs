#![no_main]
use libfuzzer_sys::fuzz_target;
use erxi::options::ExiOptions;

fuzz_target!(|data: &[u8]| {
    // Feed raw bytes through decode to get events, then re-encode them.
    // This tests the encoder with arbitrary (possibly invalid) event sequences.
    if let Ok((events, _opts)) = erxi::decode(data) {
        let _ = erxi::encoder::encode(&events, &ExiOptions::default());
    }
});
