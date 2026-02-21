#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let mut reader = erxi::bitstream::BitReader::new(data);
    let _ = erxi::header::decode(&mut reader, true);
});
