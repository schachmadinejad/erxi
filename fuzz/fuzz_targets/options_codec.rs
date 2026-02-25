#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = erxi::options_codec::decode_from_slice(data);
});
