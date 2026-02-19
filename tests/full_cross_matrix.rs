use std::process::Command;

#[test]
fn run_full_cross_matrix() {
    let status = Command::new("bash")
        .arg("-lc")
        .arg("scripts/setup.sh --export")
        .status()
        .expect("run setup");
    assert!(status.success(), "setup failed");

    let status = Command::new("bash")
        .arg("-lc")
        .arg("source target/cross_env.sh && scripts/run.sh --skip-build")
        .status()
        .expect("run matrix");
    assert!(status.success(), "matrix failed");
}
