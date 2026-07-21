pub struct Scoping {
    pub scope_counter: u64,
    pub scopes: FxHashMap<u64, MiddleScope>,
    pub loaded_scopes: FxHashSet<u64>,
    pub loop_stack: Vec<LoopContext>,
}

#[derive(Debug, Clone, Default)]
pub struct LoopContext {
    pub label: Option<String>,
    pub result_target: Option<ParserText>,
    pub broke_target: Option<ParserText>,
    pub continue_inject: Option<Node>,
    pub scope_id: u64,
}

fn empty_scope() -> &'static MiddleScope {
    static EMPTY: std::sync::OnceLock<MiddleScope> = std::sync::OnceLock::new();
    EMPTY.get_or_init(|| MiddleScope {
        id: 0,
        parent: None,
        mappings: FxHashMap::default(),
        macros: FxHashMap::default(),
        macro_args: FxHashMap::default(),
        children: FxHashMap::default(),
        namespace: "empty".to_string(),
        path: PathBuf::new(),
        defined: Vec::new(),
        defers: Vec::new(),
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeMacro {
    pub name: String,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
    pub body: Vec<Node>,
    pub create_new_scope: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub id: u64,
    pub parent: Option<u64>,
    pub mappings: FxHashMap<String, String>,
    pub macros: FxHashMap<String, ScopeMacro>,
    pub macro_args: FxHashMap<String, Node>,
    pub children: FxHashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
    pub defined: Vec<String>,
    pub defers: Vec<Node>,
}
