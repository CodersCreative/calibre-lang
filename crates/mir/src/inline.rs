use crate::ast::{MiddleNode, MiddleNodeType};
use rustc_hash::FxHashMap;

struct InlineFn {
    params: Vec<String>,
    body: MiddleNode,
}

pub fn inline_small_calls(root: &mut MiddleNode, max_nodes: usize) {
    let mut inline_map: FxHashMap<String, InlineFn> = FxHashMap::default();
    collect_inlineable(root, &mut inline_map, max_nodes);
    inline_in_node(root, &inline_map);
}

fn collect_inlineable(node: &MiddleNode, map: &mut FxHashMap<String, InlineFn>, max_nodes: usize) {
    match &node.node_type {
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            for stmt in body {
                collect_inlineable(stmt, map, max_nodes);
            }
        }
        MiddleNodeType::VariableDeclaration {
            identifier, value, ..
        } => {
            if let MiddleNodeType::FunctionDeclaration {
                parameters, body, ..
            } = &value.node_type
                && let Some(expr) = extract_single_return_expr(body)
                && !&expr.calls_self(&identifier.text)
                && expr.len() <= max_nodes
            {
                let params = parameters.iter().map(|(p, _, _)| p.text.clone()).collect();
                map.insert(identifier.text.clone(), InlineFn { params, body: expr });
            }
        }
        _ => {}
    }
}

fn extract_single_return_expr(body: &MiddleNode) -> Option<MiddleNode> {
    match &body.node_type {
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            if body.len() != 1 {
                return None;
            }

            match &body[0].node_type {
                MiddleNodeType::Return { value: Some(expr) } => Some((**expr).clone()),
                _ => None,
            }
        }
        MiddleNodeType::Return { value: Some(expr) } => Some((**expr).clone()),
        _ => None,
    }
}

fn inline_in_node(node: &mut MiddleNode, map: &FxHashMap<String, InlineFn>) {
    match &mut node.node_type {
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            for stmt in body {
                inline_in_node(stmt, map);
            }
        }
        MiddleNodeType::VariableDeclaration { value, .. } => inline_in_node(value, map),
        MiddleNodeType::AssignmentExpression { identifier, value } => {
            inline_in_node(identifier, map);
            inline_in_node(value, map);
        }
        MiddleNodeType::CallExpression { caller, args } => {
            inline_in_node(caller, map);
            for a in args.iter_mut() {
                inline_in_node(a, map);
            }
            if let MiddleNodeType::Identifier(id) = &caller.node_type
                && let Some(inline_fn) = map.get(&id.text)
                && inline_fn.params.len() == args.len()
            {
                let mut replacements: FxHashMap<String, MiddleNode> = FxHashMap::default();
                for (param, arg) in inline_fn.params.iter().zip(args.iter()) {
                    replacements.insert(param.clone(), arg.clone());
                }
                let mut inlined = inline_fn.body.clone();
                inlined.substitute(&replacements);
                *node = inlined;
            }
        }
        MiddleNodeType::Return { value } => {
            if let Some(v) = value.as_mut() {
                inline_in_node(v, map);
            }
        }
        MiddleNodeType::BinaryExpression { left, right, .. }
        | MiddleNodeType::ComparisonExpression { left, right, .. }
        | MiddleNodeType::BooleanExpression { left, right, .. } => {
            inline_in_node(left, map);
            inline_in_node(right, map);
        }
        MiddleNodeType::AsExpression { value, .. }
        | MiddleNodeType::IsExpression { value, .. }
        | MiddleNodeType::NegExpression { value }
        | MiddleNodeType::RefStatement { value, .. }
        | MiddleNodeType::DerefStatement { value }
        | MiddleNodeType::DebugExpression { value, .. } => inline_in_node(value, map),
        MiddleNodeType::ListLiteral(_, values) => {
            for v in values {
                inline_in_node(v, map);
            }
        }
        MiddleNodeType::RangeDeclaration { from, to, .. } => {
            inline_in_node(from, map);
            inline_in_node(to, map);
        }
        MiddleNodeType::LoopDeclaration { state, body, .. } => {
            if let Some(s) = state.as_mut() {
                inline_in_node(s, map);
            }
            inline_in_node(body, map);
        }
        MiddleNodeType::MemberExpression { path } => {
            for (n, _) in path.iter_mut() {
                inline_in_node(n, map);
            }
        }
        MiddleNodeType::EnumExpression { data, .. } => {
            if let Some(d) = data.as_mut() {
                inline_in_node(d, map);
            }
        }
        _ => {}
    }
}
