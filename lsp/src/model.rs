use super::*;

pub(crate) const MAX_OPEN_DOCUMENTS: usize = 512;
pub(crate) const CLEANUP_MS: u64 = 150;

pub(crate) const KEYWORDS: &[&str] = &[
    "fn", "let", "mut", "const", "struct", "enum", "trait", "impl", "if", "else", "match", "for",
    "while", "in", "return", "break", "continue", "try", "as", "use", "extern", "type", "range",
];

pub(crate) fn keyword_doc(keyword: &str) -> &'static str {
    match keyword {
        "fn" => "Declare a function.",
        "let" => "Declare an immutable local variable.",
        "mut" => "Declare or mark a mutable binding.",
        "const" => "Declare a constant value.",
        "struct" => "Declare a struct type.",
        "enum" => "Declare an enum type.",
        "trait" => "Declare a trait.",
        "impl" => "Implement methods or trait members.",
        "if" => "Conditional branching.",
        "else" => "Alternative branch for if.",
        "match" => "Pattern matching expression.",
        "for" => "Iteration loop.",
        "while" => "Condition-based loop.",
        "in" => "Iterator membership in loops/patterns.",
        "return" => "Return from the current function.",
        "break" => "Exit the nearest loop.",
        "continue" => "Skip to next loop iteration.",
        "try" => "Error-handling expression.",
        "as" => "Type cast operation.",
        "use" => "Import module or symbols.",
        "extern" => "Declare external/native functions.",
        "type" => "Declare a type alias/new type.",
        "range" => "Construct a range expression.",
        _ => "Language keyword.",
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DocumentState {
    pub(crate) version: i32,
    pub(crate) text: String,
    pub(crate) latest_diagnostics_job: u64,
}

#[derive(Debug)]
pub(crate) struct CalibreLanguageServer {
    pub(crate) client: ClientSocket,
    pub(crate) documents: HashMap<Url, DocumentState>,
    pub(crate) next_diagnostics_job: u64,
}

#[derive(Debug)]
pub(crate) struct DiagnosticsReadyEvent {
    pub(crate) uri: Url,
    pub(crate) version: i32,
    pub(crate) job_id: u64,
    pub(crate) diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub(crate) struct DiagnosticsDebounceElapsedEvent {
    pub(crate) uri: Url,
    pub(crate) version: i32,
    pub(crate) job_id: u64,
}

pub(crate) enum CompletionContext {
    Global { prefix: String },
    Member { base_expr: String, prefix: String },
}
