// src/main.rs (single-file master for simplicity)
// Unified Rust Compliance Framework
// Features: streaming byte-safe compliance checks, domain/namespace rules, ALN report,
// stability/perf guards, CLI + library mode, async where useful.

use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use std::time::{Duration, Instant};

use clap::{Parser, ValueEnum};
use regex::Regex;
use sha2::{Digest, Sha256};
use tokio::runtime::Builder as TokioBuilder;

// ----------------------------- CLI -----------------------------
#[derive(Parser, Debug)]
#[command(name = "aln-compliance" )]
#[command(about = "AI/IoT/Domain compliance checker (byte-safe)")]
struct Cli {
    /// Input source (file). If omitted, reads from STDIN
    #[arg(short, long)]
    input: Option<PathBuf>,

    /// Output ALN report path
    #[arg(short, long, default_value = "compliance_report.aln")] 
    output: PathBuf,

    /// Performance profile
    #[arg(long, value_enum, default_value_t = PerfProfile::Balanced)]
    profile: PerfProfile,

    /// Fail if any violation is found (exit code 2)
    #[arg(long, default_value_t = false)]
    strict_exit: bool,

    /// Maximum processing time (e.g., 5s, 120s). 0 for unlimited.
    #[arg(long, default_value = "10s")]
    max_time: humantime::Duration,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum PerfProfile { Fast, Balanced, Thorough }

// ------------------------ Core Types ---------------------------
#[derive(Debug, Clone)]
pub struct Finding {
    pub code: String,
    pub severity: Severity,
    pub message: String,
    pub offsets: Option<(usize, usize)>,
    pub context: Option<String>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity { Info, Warn, Error }

#[derive(Debug, Default)]
struct Report {
    meta: BTreeMap<String, String>,
    findings: Vec<Finding>,
}

impl Report {
    fn add(&mut self, f: Finding) { self.findings.push(f); }
    fn error(&mut self, code: &str, msg: &str, ctx: Option<String>) {
        self.add(Finding{ code: code.into(), severity: Severity::Error, message: msg.into(), offsets: None, context: ctx });
    }
    fn warn(&mut self, code: &str, msg: &str, ctx: Option<String>) {
        self.add(Finding{ code: code.into(), severity: Severity::Warn, message: msg.into(), offsets: None, context: ctx });
    }
    fn info(&mut self, code: &str, msg: &str, ctx: Option<String>) {
        self.add(Finding{ code: code.into(), severity: Severity::Info, message: msg.into(), offsets: None, context: ctx });
    }
}

// ------------------------ Namespaces ---------------------------
/// Canonical list of namespaces / labels the user asked us to cover.
fn target_namespaces() -> BTreeSet<&'static str> {
    [
        // Domains / brands / spaces (verbatim where possible; normalized safely)
        "Bit.Hub", "bit", "Bit.Bots", ".bit.bots", ".bitbot-runners", ".bit.bot-runners",
        "ALNFantasia", "fan.asia", "fan.asia.tools", "ALN", "Lisp", "LOLCODE", "Batchfile",
        "CyberOrganic.os", "Data_lake", "Super-Nova", "Virta-Sysfuck", "Virta-Netshithublol",
        "Apocalitz", "1p1-networking", "truncateduserinputs", "youtube",
        // from noisy payload—kept as opaque labels
        "Bit.Coin", ".bit.coin", ".bit.pornhubxhamster.com.bit",
    ].into_iter().collect()
}

// ---------------------- Byte-Safe Reader -----------------------
fn read_all_bytes(input: &Option<PathBuf>) -> io::Result<Vec<u8>> {
    let mut buf = Vec::with_capacity(1 << 20);
    match input {
        Some(p) => { File::open(p)?.read_to_end(&mut buf)?; },
        None => { io::stdin().read_to_end(&mut buf)?; },
    }
    Ok(buf)
}

// ---------------------- Normalization -------------------------
fn normalize_lossy(bytes: &[u8]) -> String {
    // Lossy UTF-8 for inspection, safe against invalid bytes
    String::from_utf8_lossy(bytes).to_string()
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

// ---------------------- Rule Engine ---------------------------
trait Rule { fn name(&self) -> &'static str; fn eval(&self, bytes: &[u8], text: &str, rep: &mut Report); }

struct RuleZipHeader;
impl Rule for RuleZipHeader {
    fn name(&self) -> &'static str { "zip_header" }
    fn eval(&self, bytes: &[u8], _text: &str, rep: &mut Report) {
        // Detect zip local file header PK\x03\x04 or central directory PK\x01\x02
        if bytes.windows(4).any(|w| w == [0x50,0x4B,0x03,0x04]) {
            rep.warn("ARCHIVE_EMBEDDED", "Embedded ZIP local header detected (PK\x03\x04)", None);
        }
        if bytes.windows(4).any(|w| w == [0x50,0x4B,0x01,0x02]) {
            rep.warn("ARCHIVE_CENTRALDIR", "Embedded ZIP central directory detected (PK\x01\x02)", None);
        }
    }
}

struct RuleControlChars;
impl Rule for RuleControlChars {
    fn name(&self) -> &'static str { "control_chars" }
    fn eval(&self, bytes: &[u8], _text: &str, rep: &mut Report) {
        let ctrl = bytes.iter().filter(|b| b.is_ascii_control() && **b != b'\n' && **b != b'\t' && **b != b'\r').count();
        if ctrl > 0 { rep.warn("CONTROL_CHARS", &format!("{} control bytes present" , ctrl), None); }
    }
}

struct RuleNamespaceMentions { re_token: Regex }
impl RuleNamespaceMentions {
    fn new() -> Self {
        Self { re_token: Regex::new(r"[A-Za-z0-9_\.\-/]+").unwrap() }
    }
}
impl Rule for RuleNamespaceMentions {
    fn name(&self) -> &'static str { "namespace_mentions" }
    fn eval(&self, _bytes: &[u8], text: &str, rep: &mut Report) {
        let targets = target_namespaces();
        let mut seen = BTreeSet::new();
        for m in self.re_token.find_iter(text) {
            let token = m.as_str();
            if targets.contains(token) {
                seen.insert(token.to_string());
            }
        }
        for t in seen { rep.info("NAMESPACE", &format!("Target namespace referenced: {}", t), None); }
        if rep.findings.iter().all(|f| f.code != "NAMESPACE") {
            rep.warn("NAMESPACE_MISSING", "No target namespaces referenced", None);
        }
    }
}

struct RuleLanguageHints;
impl Rule for RuleLanguageHints {
    fn name(&self) -> &'static str { "language_hints" }
    fn eval(&self, _bytes: &[u8], text: &str, rep: &mut Report) {
        let langs = ["Lisp","LOLCODE","Batchfile","ALN","Scala","Rust","Java","Python","Mojo"];
        let mut hits = vec![];
        for l in langs { if text.to_lowercase().contains(&l.to_lowercase()) { hits.push(l); } }
        if !hits.is_empty() { rep.info("LANG_HINTS", &format!("Language markers: {:?}", hits), None); }
    }
}

struct RulePerfBudget { max_bytes: usize }
impl Rule for RulePerfBudget {
    fn name(&self) -> &'static str { "perf_budget" }
    fn eval(&self, bytes: &[u8], _text: &str, rep: &mut Report) {
        if bytes.len() > self.max_bytes {
            rep.warn("PERF_BUDGET", &format!("Input size {} exceeds budget {} bytes", bytes.len(), self.max_bytes), None);
        }
    }
}

struct RuleTruncatedInput;
impl Rule for RuleTruncatedInput {
    fn name(&self) -> &'static str { "truncated_inputs" }
    fn eval(&self, bytes: &[u8], text: &str, rep: &mut Report) {
        // Heuristic: if text has many replacement characters or bytes end with an incomplete UTF-8 sequence
        let repls = text.matches('\u{FFFD}').count();
        if repls > 0 { rep.warn("UTF8_LOSSY", &format!("{} invalid UTF-8 sequences replaced", repls), None); }
        if !bytes.is_empty() && (bytes[bytes.len()-1] & 0b1100_0000) == 0b1100_0000 {
            rep.warn("TRUNCATED_END", "Input may be truncated mid-codepoint", None);
        }
    }
}

// ---------------------- Engine Runner -------------------------
struct Engine { rules: Vec<Box<dyn Rule + Send + Sync>> }
impl Engine {
    fn new(profile: PerfProfile) -> Self {
        let mut rules: Vec<Box<dyn Rule + Send + Sync>> = vec![
            Box::new(RuleZipHeader),
            Box::new(RuleControlChars),
            Box::new(RuleNamespaceMentions::new()),
            Box::new(RuleLanguageHints),
            Box::new(RuleTruncatedInput),
        ];
        let budget = match profile {
            PerfProfile::Fast => 2<<20,        // 2 MiB
            PerfProfile::Balanced => 8<<20,    // 8 MiB
            PerfProfile::Thorough => 64<<20,   // 64 MiB
        };
        rules.push(Box::new(RulePerfBudget{ max_bytes: budget }));
        Self { rules }
    }

    fn run(&self, bytes: &[u8], deadline: Option<Instant>) -> Report {
        let text = normalize_lossy(bytes);
        let mut rep = Report::default();
        rep.meta.insert("sha256".into(), sha256_hex(bytes));
        rep.meta.insert("size_bytes".into(), bytes.len().to_string());
        rep.meta.insert("engine".into(), "ALN-Rust-Compliance/1.0".into());
        rep.meta.insert("namespaces".into(), target_namespaces().into_iter().collect::<Vec<_>>().join(","));
        for r in &self.rules {
            if let Some(d) = deadline { if Instant::now() > d { rep.warn("TIMEOUT", "Rule evaluation deadline reached", None); break; } }
            r.eval(bytes, &text, &mut rep);
        }
        rep
    }
}

// ---------------------- Reporting (ALN-like) ------------------
fn write_aln_report(path: &PathBuf, rep: &Report) -> io::Result<()> {
    use std::io::Write;
    let mut f = File::create(path)?;
    writeln!(f, "@REPORT compliance_report {{")?;
    writeln!(f, "  @META {{")?;
    for (k,v) in &rep.meta { writeln!(f, "    {}: {}", k, v)?; }
    writeln!(f, "  }}")?;
    writeln!(f, "  @FINDINGS [")?;
    for (i,fd) in rep.findings.iter().enumerate() {
        writeln!(f, "    {{ id: {}, code: {}, severity: {:?}, msg: {:?} }},", i+1, fd.code, fd.severity, fd.message)?;
    }
    writeln!(f, "  ]")?;
    writeln!(f, "}}")?;
    Ok(())
}

// ---------------------- Main ----------------------------------
fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Build a minimal Tokio runtime for potential async adapters (future-proof)
    let _rt = TokioBuilder::new_current_thread()
        .enable_all()
        .build()?;

    let bytes = read_all_bytes(&cli.input)?;
    let engine = Engine::new(cli.profile);

    let max_ns = cli.max_time.as_secs_f64();
    let deadline = if max_ns > 0.0 { Some(Instant::now() + Duration::from_secs_f64(max_ns)) } else { None };
    let rep = engine.run(&bytes, deadline);

    write_aln_report(&cli.output, &rep)?;

    let has_errors = rep.findings.iter().any(|f| f.severity == Severity::Error);
    if cli.strict_exit && has_errors { std::process::exit(2); }
    Ok(())
}

// ---------------------- Tests ---------------------------------
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_zip_headers() {
        let data = b"hello PK\x03\x04 world";
        let r = Engine::new(PerfProfile::Balanced).run(data, None);
        assert!(r.findings.iter().any(|f| f.code == "ARCHIVE_EMBEDDED"));
    }

    #[test]
    fn flags_control_chars() {
        let data = b"safe\x00text";
        let r = Engine::new(PerfProfile::Balanced).run(data, None);
        assert!(r.findings.iter().any(|f| f.code == "CONTROL_CHARS"));
    }

    #[test]
    fn notes_namespaces() {
        let data = b"Contact fan.asia and ALN.";
        let r = Engine::new(PerfProfile::Balanced).run(data, None);
        assert!(r.findings.iter().any(|f| f.code == "NAMESPACE"));
    }
}
