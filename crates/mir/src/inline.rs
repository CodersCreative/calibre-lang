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
            {
                if let Some(expr) = extract_single_return_expr(body) {
                    let name = identifier.text.clone();
                    if !contains_self_call(&expr, &name) && count_nodes(&expr) <= max_nodes {
                        let params = parameters.iter().map(|(p, _)| p.text.clone()).collect();
                        map.insert(name, InlineFn { params, body: expr });
                    }
                }
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

fn contains_self_call(node: &MiddleNode, name: &str) -> bool {
    match &node.node_type {
        MiddleNodeType::Identifier(id) => id.text == name,
        MiddleNodeType::CallExpression { caller, args } => {
            if contains_self_call(caller, name) {
                return true;
            }
            args.iter().any(|a| contains_self_call(a, name))
        }
        MiddleNodeType::FunctionDeclaration { .. } => false,
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            body.iter().any(|n| contains_self_call(n, name))
        }
        MiddleNodeType::Return { value } => value
            .as_ref()
            .map_or(false, |v| contains_self_call(v, name)),
        MiddleNodeType::VariableDeclaration { value, .. } => contains_self_call(value, name),
        MiddleNodeType::AssignmentExpression { identifier, value } => {
            contains_self_call(identifier, name) || contains_self_call(value, name)
        }
        MiddleNodeType::BinaryExpression { left, right, .. }
        | MiddleNodeType::ComparisonExpression { left, right, .. }
        | MiddleNodeType::BooleanExpression { left, right, .. } => {
            contains_self_call(left, name) || contains_self_call(right, name)
        }
        MiddleNodeType::AsExpression { value, .. }
        | MiddleNodeType::NegExpression { value }
        | MiddleNodeType::RefStatement { value, .. }
        | MiddleNodeType::DerefStatement { value }
        | MiddleNodeType::DebugExpression { value, .. } => contains_self_call(value, name),
        MiddleNodeType::ListLiteral(_, values) => {
            values.iter().any(|v| contains_self_call(v, name))
        }
        MiddleNodeType::RangeDeclaration { from, to, .. } => {
            contains_self_call(from, name) || contains_self_call(to, name)
        }
        MiddleNodeType::LoopDeclaration { state, body, .. } => {
            state
                .as_ref()
                .map_or(false, |s| contains_self_call(s, name))
                || contains_self_call(body, name)
        }
        MiddleNodeType::MemberExpression { path } => {
            path.iter().any(|(n, _)| contains_self_call(n, name))
        }
        MiddleNodeType::EnumExpression { data, .. } => {
            data.as_ref().map_or(false, |d| contains_self_call(d, name))
        }
        _ => false,
    }
}

fn count_nodes(node: &MiddleNode) -> usize {
    let mut count = 1;
    match &node.node_type {
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            for n in body {
                count += count_nodes(n);
            }
        }
        MiddleNodeType::Return { value } => {
            if let Some(v) = value.as_ref() {
                count += count_nodes(v);
            }
        }
        MiddleNodeType::VariableDeclaration { value, .. } => count += count_nodes(value),
        MiddleNodeType::AssignmentExpression { identifier, value } => {
            count += count_nodes(identifier);
            count += count_nodes(value);
        }
        MiddleNodeType::CallExpression { caller, args } => {
            count += count_nodes(caller);
            for a in args {
                count += count_nodes(a);
            }
        }
        MiddleNodeType::BinaryExpression { left, right, .. }
        | MiddleNodeType::ComparisonExpression { left, right, .. }
        | MiddleNodeType::BooleanExpression { left, right, .. } => {
            count += count_nodes(left);
            count += count_nodes(right);
        }
        MiddleNodeType::AsExpression { value, .. }
        | MiddleNodeType::NegExpression { value }
        | MiddleNodeType::RefStatement { value, .. }
        | MiddleNodeType::DerefStatement { value }
        | MiddleNodeType::DebugExpression { value, .. } => count += count_nodes(value),
        MiddleNodeType::ListLiteral(_, values) => {
            for v in values {
                count += count_nodes(v);
            }
        }
        MiddleNodeType::RangeDeclaration { from, to, .. } => {
            count += count_nodes(from);
            count += count_nodes(to);
        }
        MiddleNodeType::LoopDeclaration { state, body, .. } => {
            if let Some(s) = state.as_ref() {
                count += count_nodes(s);
            }
            count += count_nodes(body);
        }
        MiddleNodeType::MemberExpression { path } => {
            for (n, _) in path {
                count += count_nodes(n);
            }
        }
        MiddleNodeType::EnumExpression { data, .. } => {
            if let Some(d) = data.as_ref() {
                count += count_nodes(d);
            }
        }
        _ => {}
    }
    count
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
            if let MiddleNodeType::Identifier(id) = &caller.node_type {
                if let Some(inline_fn) = map.get(&id.text) {
                    if inline_fn.params.len() == args.len() {
                        let mut replacements: FxHashMap<String, MiddleNode> = FxHashMap::default();
                        for (param, arg) in inline_fn.params.iter().zip(args.iter()) {
                            replacements.insert(param.clone(), arg.clone());
                        }
                        let mut inlined = inline_fn.body.clone();
                        substitute_idents(&mut inlined, &replacements);
                        *node = inlined;
                    }
                }
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

fn substitute_idents(node: &mut MiddleNode, repl: &FxHashMap<String, MiddleNode>) {
    match &mut node.node_type {
        MiddleNodeType::Identifier(id) => {
            if let Some(replacement) = repl.get(&id.text) {
                *node = replacement.clone();
            }
        }
        MiddleNodeType::FunctionDeclaration { .. } => {}
        MiddleNodeType::ScopeDeclaration { body, .. } => {
            for stmt in body {
                substitute_idents(stmt, repl);
            }
        }
        MiddleNodeType::VariableDeclaration { value, .. } => substitute_idents(value, repl),
        MiddleNodeType::AssignmentExpression { identifier, value } => {
            substitute_idents(identifier, repl);
            substitute_idents(value, repl);
        }
        MiddleNodeType::CallExpression { caller, args } => {
            substitute_idents(caller, repl);
            for a in args.iter_mut() {
                substitute_idents(a, repl);
            }
        }
        MiddleNodeType::Return { value } => {
            if let Some(v) = value.as_mut() {
                substitute_idents(v, repl);
            }
        }
        MiddleNodeType::BinaryExpression { left, right, .. }
        | MiddleNodeType::ComparisonExpression { left, right, .. }
        | MiddleNodeType::BooleanExpression { left, right, .. } => {
            substitute_idents(left, repl);
            substitute_idents(right, repl);
        }
        MiddleNodeType::AsExpression { value, .. }
        | MiddleNodeType::NegExpression { value }
        | MiddleNodeType::RefStatement { value, .. }
        | MiddleNodeType::DerefStatement { value }
        | MiddleNodeType::DebugExpression { value, .. } => substitute_idents(value, repl),
        MiddleNodeType::ListLiteral(_, values) => {
            for v in values {
                substitute_idents(v, repl);
            }
        }
        MiddleNodeType::RangeDeclaration { from, to, .. } => {
            substitute_idents(from, repl);
            substitute_idents(to, repl);
        }
        MiddleNodeType::LoopDeclaration { state, body, .. } => {
            if let Some(s) = state.as_mut() {
                substitute_idents(s, repl);
            }
            substitute_idents(body, repl);
        }
        MiddleNodeType::MemberExpression { path } => {
            for (n, _) in path.iter_mut() {
                substitute_idents(n, repl);
            }
        }
        MiddleNodeType::EnumExpression { data, .. } => {
            if let Some(d) = data.as_mut() {
                substitute_idents(d, repl);
            }
        }
        _ => {}
    }
}
