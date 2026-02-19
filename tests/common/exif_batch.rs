// Gemeinsamer ExifBatch-Prozess für Cross-RTT-Tests und Full-Cross-Test.
//
// Wird per `include!` eingebunden — dadurch leben alle Funktionen im
// Namensraum des einbindenden Moduls. Benötigte Imports müssen VOR dem
// `include!` vorhanden sein:
//   use std::cell::RefCell;
//   use std::io::{BufRead, BufReader, Write};
//   use std::process::{Child, Command, Stdio};

fn exificient_jar() -> String {
    std::env::var("EXIFICIENT_JAR").unwrap_or_else(|_| {
        format!(
            "{}/tests/fixtures/exificient/exificient-1.0.4-fat.jar",
            env!("CARGO_MANIFEST_DIR")
        )
    })
}

const EXIFBATCH_CLASS: &str = "ExifBatch";
const EXIFBATCH_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tools");
const EXIFBATCH_CLASSFILE: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tools/ExifBatch.class");

struct ExifBatchProcess {
    child: Child,
    stdin: std::io::BufWriter<std::process::ChildStdin>,
    stdout: BufReader<std::process::ChildStdout>,
}

impl ExifBatchProcess {
    fn start() -> Result<Self, String> {
        eprintln!("ExifBatch: JVM start");
        if !std::path::Path::new(EXIFBATCH_CLASSFILE).exists() {
            return Err(format!(
                "ExifBatch class fehlt: {} (bitte tools/ExifBatch.java kompilieren)",
                EXIFBATCH_CLASSFILE
            ));
        }

        let mut cmd = Command::new("java");
        cmd.args([
            "-Xmx512m",
            "-cp",
            &format!("{}:{}", exificient_jar(), EXIFBATCH_DIR),
        ]);
        cmd.arg(EXIFBATCH_CLASS);
        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|e| format!("ExifBatch start: {e}"))?;

        let stdin = std::io::BufWriter::new(child.stdin.take().unwrap());
        let stdout = BufReader::new(child.stdout.take().unwrap());

        Ok(Self { child, stdin, stdout })
    }

    fn execute(&mut self, args: &[String]) -> Result<(), String> {
        let line = args.join(" ");
        writeln!(self.stdin, "{line}").map_err(|e| format!("ExifBatch write: {e}"))?;
        self.stdin.flush().map_err(|e| format!("ExifBatch flush: {e}"))?;

        let mut response = String::new();
        self.stdout
            .read_line(&mut response)
            .map_err(|e| format!("ExifBatch read: {e}"))?;

        let response = response.trim();
        if response == "OK" {
            Ok(())
        } else if let Some(err) = response.strip_prefix("ERROR: ") {
            Err(err.to_string())
        } else {
            Err(format!("ExifBatch unexpected: {response}"))
        }
    }
}

impl Drop for ExifBatchProcess {
    fn drop(&mut self) {
        // Leere Zeile senden um sauber zu beenden
        let _ = writeln!(self.stdin);
        let _ = self.stdin.flush();
        let _ = self.child.wait();
    }
}

thread_local! {
    static EXIF_BATCH: RefCell<Option<ExifBatchProcess>> = RefCell::new(None);
}

fn exif_batch_call(args: &[String]) -> Result<(), String> {
    EXIF_BATCH.with(|batch_cell| {
        {
            let mut guard = batch_cell.borrow_mut();

            // Starten falls noetig
            if guard.is_none() {
                *guard = Some(ExifBatchProcess::start().map_err(|e| format!("ExifBatch starten: {e}"))?);
            }
            let result = guard.as_mut().unwrap().execute(args);

            // Bei Kommunikationsfehler: Batch ist instabil.
            if let Err(ref e) = result {
                if e.contains("ExifBatch write")
                    || e.contains("ExifBatch read")
                    || e.contains("ExifBatch flush")
                    || e.contains("ExifBatch unexpected")
                {
                    eprintln!("ExifBatch: JVM comm error: {e}");
                }
            }
            result
        }
    })
}

fn exificient_encode(
    xml_path: &Path,
    opts: &ExiOptions,
    schema_path: &str,
    out_path: &Path,
) -> Result<(), String> {
    let mut args = vec![
        "-encode".to_string(),
        "-i".to_string(),
        xml_path.to_string_lossy().to_string(),
        "-o".to_string(),
        out_path.to_string_lossy().to_string(),
    ];
    args.extend(options_to_exificient_args(opts, schema_path));

    exif_batch_call(&args).map_err(|e| format!("exificient encode failed: {e}"))
}

fn exificient_decode(
    exi_path: &Path,
    opts: &ExiOptions,
    schema_path: &str,
    out_path: &Path,
) -> Result<(), String> {
    let mut args = vec![
        "-decode".to_string(),
        "-i".to_string(),
        exi_path.to_string_lossy().to_string(),
        "-o".to_string(),
        out_path.to_string_lossy().to_string(),
    ];
    args.extend(options_to_exificient_args(opts, schema_path));

    exif_batch_call(&args).map_err(|e| format!("exificient decode failed: {e}"))
}
