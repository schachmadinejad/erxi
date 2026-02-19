//! erxi CLI — XML <-> EXI conversion.

#[cfg(feature = "fast-alloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use clap::{Args, Parser, Subcommand};
use erxi::encoder::EncoderConfig;
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions, Preserve, SchemaId};
use erxi::xml_serializer::{events_to_pretty_xml_writer, events_to_xml_iter_fallible};
use erxi::xsd::parse_xsd_with_imports;
use erxi::Error;
use erxi::ExiEvent;
use erxi::QName;
use std::io::{IsTerminal, Read, Write};
use std::path::Path;
use std::process;

const ENCODE_FLUSH_BYTES: usize = 8 * 1024 * 1024;

fn encode_event_with_flush(
    encoder: &mut erxi::encoder::Encoder,
    writer: &mut Option<std::io::BufWriter<Box<dyn Write>>>,
    event: ExiEvent,
) -> Result<(), erxi::Error> {
    encoder.encode_event(&event)?;
    if let Some(writer) = writer.as_mut() {
        if encoder.buf_len() >= ENCODE_FLUSH_BYTES {
            encoder.flush_to(writer)?;
        }
    }
    Ok(())
}

#[derive(Parser)]
#[command(name = "erxi", about = "XML/JSON <-> EXI conversion")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Encode XML to EXI
    Encode(EncodeArgs),
    /// Decode EXI to XML
    Decode(DecodeArgs),
    /// JSON <-> EXI4JSON conversion
    Json {
        #[command(subcommand)]
        command: JsonCommand,
    },
}

#[derive(Subcommand)]
enum JsonCommand {
    /// Encode JSON to EXI4JSON
    Encode(JsonEncodeArgs),
    /// Decode EXI4JSON to JSON
    Decode(JsonDecodeArgs),
}

#[derive(Args)]
struct EncodeArgs {
    #[command(flatten)]
    common: CommonArgs,

    /// Always include options in EXI header (default: auto when non-default options)
    #[arg(long)]
    include_options: bool,

    /// Do NOT include options in EXI header (overrides --include-options and auto-detect)
    #[arg(long)]
    no_include_options: bool,

    /// Write "$EXI" cookie
    #[arg(long)]
    include_cookie: bool,
}

#[derive(Args)]
struct DecodeArgs {
    #[command(flatten)]
    common: CommonArgs,

    /// Pretty-printed XML output (2-space indent)
    #[arg(long)]
    pretty: bool,
}

#[derive(Args)]
struct JsonEncodeArgs {
    /// Input file (- for stdin)
    #[arg(short, long)]
    input: String,

    /// Output file (optional; without -o auto-derived, -o - = stdout)
    #[arg(short, long)]
    output: Option<String>,

    /// Enable EXI4JSON <other> heuristics (date/time/base64/integer/decimal)
    #[arg(long)]
    exi4json_other: bool,
}

#[derive(Args)]
struct JsonDecodeArgs {
    /// Input file (- for stdin)
    #[arg(short, long)]
    input: String,

    /// Output file (optional; without -o auto-derived, -o - = stdout)
    #[arg(short, long)]
    output: Option<String>,

    /// Pretty-printed JSON output (2-space indent)
    #[arg(long)]
    pretty: bool,
}

#[derive(Args)]
struct CommonArgs {
    /// Input file (- for stdin)
    #[arg(short, long)]
    input: String,

    /// Output file (optional; without -o auto-derived, -o - = stdout)
    #[arg(short, long)]
    output: Option<String>,

    /// Schema file (.xsd)
    #[arg(short, long)]
    schema: Option<String>,

    // -- Alignment (gegenseitig ausschliessend) --
    /// Byte alignment
    #[arg(long, conflicts_with_all = ["pre_compression", "compression"])]
    byte_aligned: bool,

    /// Pre-compression alignment
    #[arg(long, conflicts_with_all = ["byte_aligned", "compression"])]
    pre_compression: bool,

    /// DEFLATE compression
    #[arg(long, conflicts_with_all = ["byte_aligned", "pre_compression"])]
    compression: bool,

    // -- Modus --
    /// Strict mode
    #[arg(long)]
    strict: bool,

    /// Fragment mode
    #[arg(long)]
    fragment: bool,

    // -- Fidelity (preserve) --
    /// Preserve comments (CM)
    #[arg(long)]
    preserve_comments: bool,

    /// Preserve processing instructions (PI)
    #[arg(long)]
    preserve_pis: bool,

    /// Preserve DTDs and entity references (DT/ER)
    #[arg(long)]
    preserve_dtd: bool,

    /// Preserve namespace prefixes
    #[arg(long)]
    preserve_prefixes: bool,

    /// Preserve lexical values
    #[arg(long)]
    preserve_lexical: bool,

    /// Preserve insignificant whitespace (default: stripped)
    #[arg(long)]
    preserve_whitespace: bool,

    /// Enable self-contained fragments (Spec 8.5.4.4.1)
    #[arg(long)]
    self_contained: bool,

    /// Self-contained only for specific elements (URI LOCAL), repeatable
    #[arg(long, value_names = ["URI", "LOCAL"], num_args = 2, action = clap::ArgAction::Append)]
    self_contained_qname: Vec<String>,

    /// Schema ID in EXI header (Spec 5.4)
    #[arg(long, conflicts_with_all = ["schema_id_none", "schema_id_builtin"])]
    schema_id: Option<String>,

    /// Schema ID = None (xsi:nil=true)
    #[arg(long, conflicts_with_all = ["schema_id", "schema_id_builtin"])]
    schema_id_none: bool,

    /// Schema ID = BuiltinOnly (empty string)
    #[arg(long, conflicts_with_all = ["schema_id", "schema_id_none"])]
    schema_id_builtin: bool,

    /// Datatype Representation Map entry (TYPE_URI TYPE_LOCAL REP_URI REP_LOCAL), repeatable
    #[arg(long, value_names = ["TYPE_URI", "TYPE_LOCAL", "REP_URI", "REP_LOCAL"], num_args = 4, action = clap::ArgAction::Append)]
    dtrm: Vec<String>,

    // -- Erweitert --
    /// Parallel DEFLATE compression (requires --compression)
    #[arg(long, requires = "compression")]
    parallel_deflate: bool,

    /// Compression block size
    #[arg(long, default_value_t = 1_000_000)]
    block_size: u32,

    /// String table max value length
    #[arg(long)]
    value_max_length: Option<u32>,

    /// String table partitions capacity
    #[arg(long)]
    value_capacity: Option<u32>,

    /// Disable memory monitoring
    #[arg(long)]
    no_memory_monitor: bool,
}

impl CommonArgs {
    fn to_options(&self) -> ExiOptions {
        let alignment = if self.byte_aligned {
            Alignment::ByteAlignment
        } else if self.pre_compression {
            Alignment::PreCompression
        } else {
            Alignment::BitPacked
        };

        let mut opts = ExiOptions::default();
        opts.set_alignment(alignment);
        opts.set_compression(self.compression);
        opts.set_strict(self.strict);
        opts.set_fragment(self.fragment);
        opts.set_preserve(Preserve {
            comments: self.preserve_comments,
            pis: self.preserve_pis,
            dtd: self.preserve_dtd,
            prefixes: self.preserve_prefixes,
            lexical_values: self.preserve_lexical,
            whitespace: self.preserve_whitespace,
        });
        if self.self_contained || !self.self_contained_qname.is_empty() {
            opts.set_self_contained(true);
            if !self.self_contained_qname.is_empty() {
                let mut qnames = Vec::with_capacity(self.self_contained_qname.len() / 2);
                for pair in self.self_contained_qname.chunks(2) {
                    qnames.push(QName::new(pair[0].as_str(), pair[1].as_str()));
                }
                opts.set_self_contained_qnames(qnames);
            }
        }
        if self.schema_id_none {
            opts.set_schema_id(Some(SchemaId::None));
        } else if self.schema_id_builtin {
            opts.set_schema_id(Some(SchemaId::BuiltinOnly));
        } else if let Some(ref id) = self.schema_id {
            opts.set_schema_id(Some(SchemaId::Id(id.clone())));
        }
        if !self.dtrm.is_empty() {
            let mut map = Vec::with_capacity(self.dtrm.len() / 4);
            for chunk in self.dtrm.chunks(4) {
                map.push(DatatypeRepresentationMapping {
                    type_qname: QName::new(chunk[0].as_str(), chunk[1].as_str()),
                    representation_qname: QName::new(chunk[2].as_str(), chunk[3].as_str()),
                });
            }
            opts.set_datatype_representation_map(map);
        }
        opts.set_block_size(self.block_size);
        opts.set_value_max_length(self.value_max_length);
        opts.set_value_partition_capacity(self.value_capacity);
        opts
    }
}

fn read_input(path: &str) -> Result<Vec<u8>, String> {
    if path == "-" {
        if std::io::stdin().is_terminal() {
            eprintln!("Lese von stdin (Ctrl+D zum Beenden)...");
        }
        let mut buf = Vec::new();
        std::io::stdin()
            .read_to_end(&mut buf)
            .map_err(|e| format!("Lesefehler (stdin): {e}"))?;
        Ok(buf)
    } else {
        std::fs::read(path).map_err(|e| format!("Lesefehler '{}': {e}", path))
    }
}

/// Header-bezogene Fehler, bei denen ein Fallback auf CLI-Options sinnvoll ist.
fn is_header_error(e: &Error) -> bool {
    matches!(
        e,
        Error::MalformedHeader | Error::InvalidDistinguishingBits(_) | Error::UnsupportedVersion
    )
}

/// Besitzer der Decode-Eingabedaten. Haelt entweder eine Mmap oder einen Vec<u8> am Leben,
/// damit der zurueckgegebene &[u8] Slice gueltig bleibt.
enum DecodeInput {
    Buf(Vec<u8>),
    #[cfg(feature = "mmap")]
    Mmap(memmap2::Mmap),
}

impl std::ops::Deref for DecodeInput {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        match self {
            DecodeInput::Buf(v) => v,
            #[cfg(feature = "mmap")]
            DecodeInput::Mmap(m) => m,
        }
    }
}

/// Laedt die Eingabedaten fuer den Decode-Pfad.
/// Datei-Input wird Memory-Mapped (nur benoetigte Seiten im RAM).
/// stdin wird per read_to_end() gelesen (unvermeidbar, da kein fd fuer mmap).
fn load_decode_input(path: &str) -> Result<DecodeInput, String> {
    if path != "-" {
        #[cfg(feature = "mmap")]
        {
            let file = std::fs::File::open(path)
                .map_err(|e| format!("Lesefehler '{}': {e}", path))?;
            let mmap = unsafe { memmap2::Mmap::map(&file) }
                .map_err(|e| format!("Mmap-Fehler '{}': {e}", path))?;
            return Ok(DecodeInput::Mmap(mmap));
        }
        #[cfg(not(feature = "mmap"))]
        {
            let buf = std::fs::read(path)
                .map_err(|e| format!("Lesefehler '{}': {e}", path))?;
            return Ok(DecodeInput::Buf(buf));
        }
    }
    let buf = read_input("-")?;
    Ok(DecodeInput::Buf(buf))
}

fn main() {
    let cli = Cli::parse();

    if let Err(e) = run(cli) {
        eprintln!("Fehler: {e}");
        process::exit(1);
    }
}

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        Command::Encode(args) => run_encode(args),
        Command::Decode(args) => run_decode(args),
        Command::Json { command } => run_json(command),
    }
}

/// Prüft ob Streaming-Modus möglich ist:
/// - Kein PreCompression/Compression
/// - Input ist Datei (nicht stdin), da bei DTD-Fallback die Datei erneut gelesen
///   werden muss (Batch-Pfad)
fn can_stream_file(args: &EncodeArgs, opts: &ExiOptions) -> bool {
    !opts.compression()
        && !matches!(opts.alignment(), Alignment::PreCompression)
        && args.common.input != "-"
}

fn can_stream_stdin(opts: &ExiOptions) -> bool {
    !opts.compression() && !matches!(opts.alignment(), Alignment::PreCompression)
}

fn run_encode(args: EncodeArgs) -> Result<(), String> {
    let opts = args.common.to_options();

    opts.validate()
        .map_err(|e| format!("Ungueltige Optionen: {e}"))?;

    let output_path = resolve_output_path(args.common.output.as_deref(), &args.common.input, "exi")?;
    let to_stdout = output_path == "-";
    let tmp_path = if to_stdout { None } else { Some(format!("{output_path}.tmp")) };
    // Pfad fuer BufWriter: tmp-Datei oder stdout ("-")
    let writer_target = tmp_path.as_deref().unwrap_or("-");

    let schema = args.common.schema.as_ref()
        .map(|p| parse_xsd_with_imports(Path::new(p)))
        .transpose()
        .map_err(|e| format!("Schema-Parse-Fehler: {e}"))?;
    validate_schema_option_requirements(&opts, args.common.schema.is_some())?;

    // Auto-Detect: Options in Header schreiben wenn nicht-default Optionen aktiv.
    // --no-include-options ueberschreibt alles, --include-options forciert.
    let include_options = if args.no_include_options {
        false
    } else if args.include_options {
        true
    } else {
        opts.compression()
            || matches!(opts.alignment(), Alignment::PreCompression | Alignment::ByteAlignment)
            || opts.strict()
            || opts.fragment()
            || opts.preserve().has_header_relevant_flags()
            || opts.block_size() != 1_000_000
            || opts.value_max_length().is_some()
            || opts.value_partition_capacity().is_some()
            || opts.self_contained()
            || opts.schema_id().is_some()
            || !opts.datatype_representation_map().is_empty()
    };

    let config = EncoderConfig {
        include_options,
        include_cookie: args.include_cookie,
    };

    // Streaming von stdin (keine DTD-Entity-Aufloesung wie beim alten stdin-Pfad)
    if args.common.input == "-" && can_stream_stdin(&opts) {
        let stdin = std::io::stdin();
        let result = erxi::streaming::encode_xml_stream_with_config_and_dtd_guard(
            stdin.lock(),
            create_buf_writer(writer_target)?,
            &opts,
            schema.as_ref(),
            config.clone(),
            None,
            create_memory_monitor(&args.common),
            false,
        );
        return finish_tmp_output(result.map_err(|e| format!("Encode-Fehler: {e}")), tmp_path.as_deref(), &output_path);
    }

    // Streaming-Pfad fuer grosse Dateien (begrenzter Speicher)
    if can_stream_file(&args, &opts) {
        let input_path = Path::new(&args.common.input);
        let result = erxi::streaming::encode_xml_stream_with_config(
            std::fs::File::open(input_path)
                .map_err(|e| format!("Lesefehler '{}': {e}", args.common.input))?,
            create_buf_writer(writer_target)?,
            &opts,
            schema.as_ref(),
            config.clone(),
            input_path.parent(),
            create_memory_monitor(&args.common),
        );
        match result {
            Ok(()) => {
                return finish_tmp_output(Ok(()), tmp_path.as_deref(), &output_path);
            }
            Err(erxi::Error::DtdRequiresBatchApi) => {
                eprintln!("Hinweis: DTD-Entities erkannt, wechsle auf Batch-Modus...");
            }
            Err(e) => {
                return finish_tmp_output(
                    Err(format!("Encode-Fehler: {e}")),
                    tmp_path.as_deref(),
                    &output_path,
                );
            }
        }
        // DTD-Fallback: tmp-Datei aufraumen bevor Batch-Pfad beginnt
        if let Some(ref path) = tmp_path {
            let _ = std::fs::remove_file(path);
        }
    }

    // Batch-Pfad: Callback-basiert (kein Events Vec im Speicher)

    let mut encoder = create_configured_encoder(
        &args.common, opts.clone(), config.clone(), schema.as_ref(),
    )?;

    let wants_stream_output = !opts.compression()
        && !matches!(opts.alignment(), Alignment::PreCompression);
    let mut output_writer: Option<std::io::BufWriter<Box<dyn Write>>> = if wants_stream_output {
        Some(create_buf_writer(writer_target)?)
    } else {
        None
    };

    if args.common.input != "-" {
        let result = erxi::xml::emit_xml_events_from_file_cb(
            Path::new(&args.common.input),
            &opts,
            |event| encode_event_with_flush(&mut encoder, &mut output_writer, event),
        );
        match result {
            Ok(()) => {}
            Err(erxi::Error::DtdRequiresBatchApi) => {
                eprintln!("Hinweis: DTD-Entities erkannt, wechsle auf Batch-Modus...");
                if let Some(path) = tmp_path.as_deref() {
                    output_writer.take();
                    let _ = std::fs::remove_file(path);
                    output_writer = Some(create_buf_writer(path)?);
                }
                // Neuen Encoder — der alte hat bereits inkonsistenten State
                encoder = create_configured_encoder(
                    &args.common, opts.clone(), config, schema.as_ref(),
                )?;
                erxi::xml::emit_xml_events_from_file_batch_cb(
                    Path::new(&args.common.input),
                    &opts,
                    |event| encode_event_with_flush(&mut encoder, &mut output_writer, event),
                ).map_err(|e| format!("Encode-Fehler: {e}"))?;
            }
            Err(e) => return Err(format!("Encode-Fehler: {e}")),
        }
    } else {
        let data = read_input("-")?;
        let xml = String::from_utf8(data).map_err(|e| format!("UTF-8-Fehler: {e}"))?;
        erxi::xml::emit_xml_events_from_str_cb(
            &xml,
            &opts,
            |event| encode_event_with_flush(&mut encoder, &mut output_writer, event),
        ).map_err(|e| format!("Encode-Fehler: {e}"))?;
    }

    if opts.compression() || matches!(opts.alignment(), Alignment::PreCompression) {
        // Compression/PreCompression erfordern vollständiges Buffering (Spec 9)
        let bytes = encoder.finish().map_err(|e| format!("Encode-Fehler: {e}"))?;
        if to_stdout {
            std::io::stdout()
                .write_all(&bytes)
                .map_err(|e| format!("Schreibfehler (stdout): {e}"))?;
        } else {
            std::fs::write(&output_path, bytes).map_err(|e| format!("Schreibfehler: {e}"))?;
        }
    } else {
        // BitPacked/ByteAlignment: direkt in Datei schreiben (kein Vec<u8> im RAM)
        let mut writer = output_writer
            .take()
            .ok_or("interner Fehler: Output-Writer fehlt")?;
        encoder.finish_to(&mut writer)
            .map_err(|e| format!("Encode-Fehler: {e}"))?;
        writer.flush().map_err(|e| format!("Schreibfehler: {e}"))?;
        if let Some(path) = tmp_path.as_deref() {
            std::fs::rename(path, &output_path)
                .map_err(|e| format!("Rename-Fehler: {e}"))?;
        }
    }

    Ok(())
}

/// Erstellt einen Decode-Iterator (Schema oder Schema-less, mit Header-Fallback).
fn create_decode_iter<'a>(
    data: &'a [u8],
    opts: ExiOptions,
    schema: Option<&erxi::SchemaInfo>,
) -> std::result::Result<(erxi::DecodeIter<'a>, ExiOptions), String> {
    if let Some(schema) = schema {
        erxi::decode_iter_with_schema(data, opts, schema)
            .map_err(|e| format!("Decode-Fehler: {e}"))
    } else {
        match erxi::decode_iter_with_options(data, opts.clone()) {
            Ok(result) => Ok(result),
            Err(e) if is_header_error(&e) => {
                eprintln!("Hinweis: Header-Decode fehlgeschlagen ({e}), versuche mit Default-Optionen...");
                erxi::decode_iter_with_options(data, ExiOptions::default())
                    .map_err(|e2| format!("Decode-Fehler: {e2}"))
            }
            Err(e) => Err(format!("Decode-Fehler: {e}")),
        }
    }
}

/// Erstellt einen MemoryMonitor (falls nicht deaktiviert).
fn create_memory_monitor(args: &CommonArgs) -> Option<erxi::MemoryMonitor> {
    if args.no_memory_monitor {
        return None;
    }
    erxi::MemoryMonitor::new()
}

/// Erstellt einen Encoder mit optionalem Schema.
fn create_encoder(
    opts: ExiOptions,
    config: EncoderConfig,
    schema: Option<&erxi::SchemaInfo>,
) -> std::result::Result<erxi::encoder::Encoder, String> {
    if let Some(schema) = schema {
        erxi::encoder::Encoder::with_schema(opts, config, schema.clone())
    } else {
        erxi::encoder::Encoder::new(opts, config)
    }.map_err(|e| format!("Encode-Fehler: {e}"))
}

/// Erstellt einen vollstaendig konfigurierten Encoder (Schema, Parallel-DEFLATE, MemoryMonitor).
fn create_configured_encoder(
    args: &CommonArgs,
    opts: ExiOptions,
    config: EncoderConfig,
    schema: Option<&erxi::SchemaInfo>,
) -> std::result::Result<erxi::encoder::Encoder, String> {
    let mut encoder = create_encoder(opts, config, schema)?;
    encoder.set_parallel_deflate(args.parallel_deflate);
    if let Some(monitor) = create_memory_monitor(args) {
        encoder.set_memory_monitor(monitor);
    }
    Ok(encoder)
}

/// Erstellt einen BufWriter fuer stdout oder eine Datei.
fn create_buf_writer(path: &str) -> Result<std::io::BufWriter<Box<dyn Write>>, String> {
    if path == "-" {
        Ok(std::io::BufWriter::new(Box::new(std::io::stdout())))
    } else {
        let file = std::fs::File::create(path)
            .map_err(|e| format!("Schreibfehler: {e}"))?;
        Ok(std::io::BufWriter::new(Box::new(file)))
    }
}

/// Schreibt Output entweder nach stdout ("-") oder atomar in eine Datei (tmp+rename).
///
/// Die Closure bekommt einen BufWriter und schreibt darin.
/// Bei Datei-Output wird erst in eine .tmp-Datei geschrieben und bei Erfolg umbenannt.
fn write_to_output(
    output_path: &str,
    write_fn: impl FnOnce(std::io::BufWriter<Box<dyn Write>>) -> Result<(), String>,
) -> Result<(), String> {
    if output_path == "-" {
        return write_fn(create_buf_writer("-")?);
    }

    let tmp_path = format!("{output_path}.tmp");
    let writer = create_buf_writer(&tmp_path)?;
    if let Err(e) = write_fn(writer) {
        let _ = std::fs::remove_file(&tmp_path);
        return Err(e);
    }
    std::fs::rename(&tmp_path, output_path)
        .map_err(|e| format!("Rename-Fehler: {e}"))
}

/// Bei Erfolg: tmp-Datei auf Ziel umbenennen. Bei Fehler: tmp-Datei loeschen.
/// Fuer stdout-Output (tmp_path=None) wird nur der Result durchgereicht.
fn finish_tmp_output(
    result: Result<(), String>,
    tmp_path: Option<&str>,
    output_path: &str,
) -> Result<(), String> {
    match (&result, tmp_path) {
        (Ok(()), Some(tmp)) => {
            std::fs::rename(tmp, output_path)
                .map_err(|e| format!("Rename-Fehler: {e}"))
        }
        (Err(_), Some(tmp)) => {
            let _ = std::fs::remove_file(tmp);
            result
        }
        _ => result,
    }
}

fn run_decode(args: DecodeArgs) -> Result<(), String> {
    let opts = args.common.to_options();

    opts.validate()
        .map_err(|e| format!("Ungueltige Optionen: {e}"))?;

    let schema = args.common.schema.as_ref()
        .map(|p| parse_xsd_with_imports(Path::new(p)))
        .transpose()
        .map_err(|e| format!("Schema-Parse-Fehler: {e}"))?;
    validate_schema_option_requirements(&opts, args.common.schema.is_some())?;

    let input = load_decode_input(&args.common.input)?;
    let data: &[u8] = &input;

    let (mut iter, _opts) = create_decode_iter(data, opts, schema.as_ref())?;
    if let Some(monitor) = create_memory_monitor(&args.common) {
        iter.set_memory_monitor(monitor);
    }

    let output_path = resolve_output_path(args.common.output.as_deref(), &args.common.input, "xml")?;

    if args.pretty {
        let mut events = Vec::new();
        for ev in iter {
            events.push(ev.map_err(|e| format!("Decode-Fehler: {e}"))?);
        }
        write_to_output(&output_path, |writer| {
            events_to_pretty_xml_writer(&events, writer)
                .map_err(|e| format!("Serialisierungs-Fehler: {e}"))
        })
    } else {
        write_to_output(&output_path, |writer| {
            events_to_xml_iter_fallible(iter, writer)
                .map_err(|e| format!("Serialisierungs-Fehler: {e}"))
        })
    }
}

fn run_json(cmd: JsonCommand) -> Result<(), String> {
    match cmd {
        JsonCommand::Encode(args) => run_json_encode(args),
        JsonCommand::Decode(args) => run_json_decode(args),
    }
}

fn run_json_encode(args: JsonEncodeArgs) -> Result<(), String> {
    let input = read_input(&args.input)?;
    let json = std::str::from_utf8(&input)
        .map_err(|e| format!("JSON muss UTF-8 sein: {e}"))?;

    let exi = erxi::encode_json_with_options(
        json,
        erxi::Exi4JsonOptions { use_other_types: args.exi4json_other },
    ).map_err(|e| format!("EXI4JSON Encode-Fehler: {e}"))?;

    let output = resolve_output_path(args.output.as_deref(), &args.input, "exi")?;

    write_to_output(&output, |mut writer| {
        writer.write_all(&exi)
            .map_err(|e| format!("Schreibfehler: {e}"))
    })
}

fn run_json_decode(args: JsonDecodeArgs) -> Result<(), String> {
    let input = load_decode_input(&args.input)?;
    let json = erxi::decode_json(&input)
        .map_err(|e| format!("EXI4JSON Decode-Fehler: {e}"))?;
    let json = if args.pretty {
        let value: serde_json::Value = serde_json::from_str(&json)
            .map_err(|e| format!("JSON parse error: {e}"))?;
        serde_json::to_string_pretty(&value)
            .map_err(|e| format!("JSON encode error: {e}"))?
    } else {
        json
    };

    let output = resolve_output_path(args.output.as_deref(), &args.input, "json")?;

    write_to_output(&output, |mut writer| {
        writer.write_all(json.as_bytes())
            .map_err(|e| format!("Schreibfehler: {e}"))?;
        writer.write_all(b"\n")
            .map_err(|e| format!("Schreibfehler: {e}"))
    })
}

/// Leitet den Output-Pfad aus der Eingabe und der gewuenschten Extension ab.
///
/// Bei explizitem `-o` wird dieser Pfad direkt verwendet. Ohne `-o` wird
/// die Extension der Eingabedatei ersetzt (bzw. angehaengt wenn keine vorhanden).
fn resolve_output_path(explicit: Option<&str>, input: &str, ext: &str) -> Result<String, String> {
    if let Some(path) = explicit {
        return Ok(path.to_string());
    }
    if input == "-" {
        return Err("ohne -o braucht es eine Eingabedatei (nicht stdin)".into());
    }
    let path = std::path::Path::new(input);
    let stem = path.file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| "ungueltiger Eingabepfad".to_string())?;
    let parent = path.parent().unwrap_or_else(|| std::path::Path::new(""));
    Ok(parent.join(format!("{stem}.{ext}")).to_string_lossy().to_string())
}

fn validate_schema_option_requirements(opts: &ExiOptions, has_schema: bool) -> Result<(), String> {
    if !has_schema {
        if matches!(opts.schema_id(), Some(SchemaId::Id(_))) {
            return Err("schema-id erfordert --schema".into());
        }
        if !opts.datatype_representation_map().is_empty() {
            return Err("dtrm erfordert --schema".into());
        }
    } else if matches!(
        opts.schema_id(),
        Some(SchemaId::None | SchemaId::BuiltinOnly)
    ) {
        return Err("schema-id none/builtin ist mit --schema nicht kompatibel".into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    fn parse_cli(args: &[&str]) -> Cli {
        Cli::try_parse_from(args).expect("CLI parse failed")
    }

    #[test]
    fn schema_id_flags_conflict() {
        let err = Cli::try_parse_from([
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--schema-id", "urn:test", "--schema-id-none",
        ]);
        assert!(err.is_err());
    }

    #[test]
    fn schema_id_builtin_flags_conflict() {
        let err = Cli::try_parse_from([
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--schema-id-builtin", "--schema-id-none",
        ]);
        assert!(err.is_err());
    }

    #[test]
    fn to_options_maps_self_contained_qname() {
        let cli = parse_cli(&[
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--self-contained-qname", "urn:a", "root",
        ]);
        let Command::Encode(args) = cli.command else {
            panic!("expected encode command");
        };
        let opts = args.common.to_options();
        assert!(opts.self_contained());
        assert_eq!(opts.self_contained_qnames().len(), 1);
        assert_eq!(&*opts.self_contained_qnames()[0].uri, "urn:a");
        assert_eq!(&*opts.self_contained_qnames()[0].local_name, "root");
    }

    #[test]
    fn to_options_maps_schema_id_and_dtrm() {
        let cli = parse_cli(&[
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--schema-id", "urn:schema:v1",
            "--dtrm", "http://www.w3.org/2001/XMLSchema", "decimal", "http://www.w3.org/2009/exi", "string",
        ]);
        let Command::Encode(args) = cli.command else {
            panic!("expected encode command");
        };
        let opts = args.common.to_options();
        assert!(matches!(opts.schema_id(), Some(SchemaId::Id(id)) if id == "urn:schema:v1"));
        assert_eq!(opts.datatype_representation_map().len(), 1);
        let entry = &opts.datatype_representation_map()[0];
        assert_eq!(&*entry.type_qname.local_name, "decimal");
        assert_eq!(&*entry.representation_qname.local_name, "string");
    }

    #[test]
    fn to_options_maps_schema_id_builtin_and_limits() {
        let cli = parse_cli(&[
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--schema-id-builtin",
            "--value-max-length", "321",
            "--value-capacity", "654",
            "--block-size", "12345",
        ]);
        let Command::Encode(args) = cli.command else {
            panic!("expected encode command");
        };
        let opts = args.common.to_options();
        assert!(matches!(opts.schema_id(), Some(SchemaId::BuiltinOnly)));
        assert_eq!(opts.value_max_length(), Some(321));
        assert_eq!(opts.value_partition_capacity(), Some(654));
        assert_eq!(opts.block_size(), 12_345);
    }

    #[test]
    fn to_options_maps_preserve_lexical_and_whitespace() {
        let cli = parse_cli(&[
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi",
            "--preserve-lexical", "--preserve-whitespace",
        ]);
        let Command::Encode(args) = cli.command else {
            panic!("expected encode command");
        };
        let opts = args.common.to_options();
        assert!(opts.preserve().lexical_values);
        assert!(opts.preserve().whitespace);
    }

    #[test]
    fn run_encode_rejects_schema_id_without_schema() {
        let cli = parse_cli(&[
            "erxi", "encode", "-i", "in.xml", "-o", "out.exi", "--schema-id", "urn:schema:v1",
        ]);
        let Command::Encode(args) = cli.command else {
            panic!("expected encode command");
        };
        let err = run_encode(args).expect_err("expected schema guard");
        assert!(err.contains("schema-id erfordert --schema"));
    }

    #[test]
    fn run_decode_rejects_dtrm_without_schema() {
        let cli = parse_cli(&[
            "erxi", "decode", "-i", "in.exi",
            "--dtrm", "http://www.w3.org/2001/XMLSchema", "decimal", "http://www.w3.org/2009/exi", "string",
        ]);
        let Command::Decode(args) = cli.command else {
            panic!("expected decode command");
        };
        let err = run_decode(args).expect_err("expected schema guard");
        assert!(err.contains("dtrm erfordert --schema"));
    }

    #[test]
    fn run_decode_rejects_schema_id_without_schema() {
        let cli = parse_cli(&[
            "erxi", "decode", "-i", "in.exi", "--schema-id", "urn:schema:v1",
        ]);
        let Command::Decode(args) = cli.command else {
            panic!("expected decode command");
        };
        let err = run_decode(args).expect_err("expected schema guard");
        assert!(err.contains("schema-id erfordert --schema"));
    }
}
