use std::{
    fmt::Write,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
    time::Instant,
};
use calibre_lir::VariableKey;
use rustc_hash::FxHashMap;
use wasm_sync::Mutex;

#[derive(Debug, Clone)]
pub enum CallEventType {
    FunctionEnter,
    FunctionExit,
    MemoizationHit,
}

#[derive(Debug, Clone)]
pub struct CallEvent {
    pub task_id: u64,
    pub function_name: Option<VariableKey>,
    pub event_type: CallEventType,
    pub timestamp: Instant,
    pub parent_call_id: Option<u64>,
    pub call_id: u64,
}

#[derive(Debug, Default)]
struct ProfilerState {
    events: Vec<CallEvent>,
    next_call_id: u64,
    // task_id -> stack of call_ids
    call_stack: FxHashMap<u64, Vec<u64>>,
}

#[derive(Debug, Clone)]
pub struct CallTreeProfiler {
    state: Arc<Mutex<ProfilerState>>,
    next_task_id: Arc<AtomicU64>,
}

impl Default for CallTreeProfiler {
    fn default() -> Self {
        Self {
            state: Arc::new(Mutex::new(ProfilerState::default())),
            next_task_id: Arc::new(AtomicU64::new(0)),
        }
    }
}

#[derive(Default)]
struct TrieNode {
    name: Option<String>,
    children: FxHashMap<String, usize>,
    duration: u128,
    task_id: u64,
}

impl CallTreeProfiler {
    pub fn generate_task_id(&self) -> u64 {
        self.next_task_id.fetch_add(1, Ordering::Relaxed)
    }

    pub fn record_function_enter(&self, task_id: u64, function_name: VariableKey) -> u64 {
        let mut state = self.state.lock().unwrap();
        
        let call_id = state.next_call_id;
        state.next_call_id += 1;

        let parent_call_id = state.call_stack.get(&task_id).and_then(|s| s.last().copied());
        state.call_stack.entry(task_id).or_default().push(call_id);

        state.events.push(CallEvent {
            task_id,
            function_name: Some(function_name),
            event_type: CallEventType::FunctionEnter,
            timestamp: Instant::now(),
            parent_call_id,
            call_id,
        });

        call_id
    }

    pub fn record_function_exit(&self, task_id: u64, call_id: u64) {
        let mut state = self.state.lock().unwrap();

        state.events.push(CallEvent {
            task_id,
            function_name: None,
            event_type: CallEventType::FunctionExit,
            timestamp: Instant::now(),
            parent_call_id: None,
            call_id,
        });

        if let Some(s) = state.call_stack.get_mut(&task_id) {
            s.pop();

            if s.is_empty() {
                state.call_stack.remove(&task_id);
            }
        }
    }

    pub fn record_memoization_hit(&self, task_id: u64, function_name: VariableKey) {
        let mut state = self.state.lock().unwrap();
        
        let call_id = state.next_call_id;
        state.next_call_id += 1;

        let parent_call_id = state.call_stack.get(&task_id).and_then(|s| s.last().copied());

        state.events.push(CallEvent {
            task_id,
            function_name: Some(function_name),
            event_type: CallEventType::MemoizationHit,
            timestamp: Instant::now(),
            parent_call_id,
            call_id,
        });
    }

    pub fn export(&self) -> String {
        let state = self.state.lock().unwrap();
        let num_calls = state.next_call_id as usize;

        let mut nodes: Vec<TrieNode> = Vec::new();
        let mut task_roots: FxHashMap<u64, usize> = FxHashMap::default();

        let mut call_to_node: Vec<usize> = vec![0; num_calls];
        let mut call_start_times: Vec<Option<Instant>> = vec![None; num_calls];

        let mut get_task_root = |task_id: u64, nodes: &mut Vec<TrieNode>| -> usize {
            *task_roots.entry(task_id).or_insert_with(|| {
                let id = nodes.len();
                nodes.push(TrieNode {
                    name: None,
                    children: FxHashMap::default(),
                    duration: 0,
                    task_id,
                });
                id
            })
        };

        for event in state.events.iter() {
            match event.event_type {
                CallEventType::FunctionEnter | CallEventType::MemoizationHit => {
                    let task_root = get_task_root(event.task_id, &mut nodes);
                    let parent_node_id = event
                        .parent_call_id
                        .map(|p| call_to_node[p as usize])
                        .unwrap_or(task_root);

                    let mut is_memo = false;

                    let name_str = event
                        .function_name
                        .as_ref()
                        .map(|x| if matches!(event.event_type, CallEventType::FunctionEnter) { 
                            x.to_string() 
                        } else {is_memo = true; format!("{x} (memo)")})
                        .unwrap_or_default();

                    let mut to_push = None;
                    let node_id = nodes.len();

                    let node_id = *nodes[parent_node_id]
                        .children
                        .entry(name_str.clone())
                        .or_insert_with(|| {
                            to_push = Some(TrieNode {
                                name: Some(name_str),
                                children: FxHashMap::default(),
                                duration: 0,
                                task_id: event.task_id,
                            });

                            node_id
                        });

                    if let Some(node) = to_push {
                        nodes.push(node);
                    }

                    call_to_node[event.call_id as usize] = node_id;
                    
                    if is_memo {
                        nodes[node_id].duration += 1;
                    } else {
                        call_start_times[event.call_id as usize] = Some(event.timestamp);
                    }
                }
                CallEventType::FunctionExit => {
                    if let Some(start_time) = call_start_times[event.call_id as usize] {
                        let duration = event.timestamp.duration_since(start_time).as_nanos();
                        let node_id = call_to_node[event.call_id as usize];
                        nodes[node_id].duration += duration;
                    }
                }
            }
        }

        let mut output = String::new();

        fn dfs(
            node_id: usize,
            nodes: &[TrieNode],
            current_stack: &mut Vec<String>,
            output: &mut String,
        ) {
            let node = &nodes[node_id];

            let mut pushed = false;
            if let Some(name) = &node.name {
                current_stack.push(name.clone());
                pushed = true;

                if node.duration > 0 {
                    let _ = write!(output, "Task({});", node.task_id);
                    for (i, frame) in current_stack.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(output, ";");
                        }
                        let _ = write!(output, "{}", frame);
                    }

                    let _ = writeln!(output, " {}", node.duration);
                }
            }

            for &child_id in node.children.values() {
                dfs(child_id, nodes, current_stack, output);
            }

            if pushed {
                current_stack.pop();
            }
        }

        let mut stack = Vec::new();
        for &root_id in task_roots.values() {
            dfs(root_id, &nodes, &mut stack, &mut output);
        }

        output
    }
}