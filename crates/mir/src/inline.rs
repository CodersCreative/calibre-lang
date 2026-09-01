use crate::ast::{
    MiddleNode, MiddleNodeType, MirAs, MirAssignment, MirBinary, MirBoolean, MirCall,
    MirComparison, MirDebug, MirDeref, MirEnum, MirField, MirFunction, MirIdentifier, MirIndex,
    MirIs, MirList, MirLoop, MirNeg, MirRange, MirRef, MirReturn, MirScopeDecl, MirVarDecl,
};
use ustr::{Ustr, UstrMap};

struct InlineFn {
    params: Vec<Ustr>,
    body: MiddleNode,
}

pub fn inline_small_calls(root: &mut MiddleNode, max_nodes: usize) {
    let mut inline_map: UstrMap<InlineFn> = UstrMap::default();
    collect_inlineable(root, &mut inline_map, max_nodes);
    inline_in_node(root, &inline_map);
}

fn collect_inlineable(node: &MiddleNode, map: &mut UstrMap<InlineFn>, max_nodes: usize) {
    match &node.node_type {
        MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
            for stmt in body {
                collect_inlineable(stmt, map, max_nodes);
            }
        }
        MiddleNodeType::VariableDeclaration(MirVarDecl {
            identifier, value, ..
        }) => {
            if let MiddleNodeType::FunctionDeclaration(MirFunction {
                parameters, body, ..
            }) = &value.node_type
                && let Some(expr) = extract_single_return_expr(body)
                && !&expr.calls_self(identifier)
                && expr.len() <= max_nodes
            {
                let params = parameters.iter().map(|(p, _, _)| *p).collect();
                map.insert(*identifier, InlineFn { params, body: expr });
            }
        }
        _ => {}
    }
}

fn extract_single_return_expr(body: &MiddleNode) -> Option<MiddleNode> {
    match &body.node_type {
        MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
            if body.len() != 1 {
                return None;
            }

            match &body[0].node_type {
                MiddleNodeType::Return(MirReturn { value: Some(expr) }) => Some((**expr).clone()),
                _ => None,
            }
        }
        MiddleNodeType::Return(MirReturn { value: Some(expr) }) => Some((**expr).clone()),
        _ => None,
    }
}

fn inline_in_node(node: &mut MiddleNode, map: &UstrMap<InlineFn>) {
    match &mut node.node_type {
        MiddleNodeType::CallExpression(MirCall { caller, args }) => {
            inline_in_node(caller, map);
            for a in args.iter_mut() {
                inline_in_node(a, map);
            }
            if let MiddleNodeType::Identifier(MirIdentifier { identifier }) = &caller.node_type
                && let Some(inline_fn) = map.get(identifier)
                && inline_fn.params.len() == args.len()
            {
                let mut replacements: UstrMap<MiddleNode> = UstrMap::default();
                for (param, arg) in inline_fn.params.iter().zip(args.iter()) {
                    replacements.insert(*param, arg.clone());
                }
                let mut inlined = inline_fn.body.clone();
                inlined.substitute(&replacements);
                *node = inlined;
            }
        }
        MiddleNodeType::Return(MirReturn { value })
        | MiddleNodeType::EnumExpression(MirEnum { data: value, .. }) => {
            if let Some(v) = value.as_mut() {
                inline_in_node(v, map);
            }
        }
        MiddleNodeType::BinaryExpression(MirBinary { left, right, .. })
        | MiddleNodeType::AssignmentExpression(MirAssignment {
            identifier: left,
            value: right,
        })
        | MiddleNodeType::IndexAccess(MirIndex {
            base: left,
            index: right,
        })
        | MiddleNodeType::ComparisonExpression(MirComparison { left, right, .. })
        | MiddleNodeType::BooleanExpression(MirBoolean { left, right, .. })
        | MiddleNodeType::RangeDeclaration(MirRange {
            from: left,
            to: right,
            ..
        }) => {
            inline_in_node(left, map);
            inline_in_node(right, map);
        }
        MiddleNodeType::AsExpression(MirAs { value, .. })
        | MiddleNodeType::IsExpression(MirIs { value, .. })
        | MiddleNodeType::NegExpression(MirNeg { value })
        | MiddleNodeType::RefStatement(MirRef { value, .. })
        | MiddleNodeType::DerefStatement(MirDeref { value })
        | MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. })
        | MiddleNodeType::DebugExpression(MirDebug { value, .. })
        | MiddleNodeType::FieldAccess(MirField { base: value, .. }) => inline_in_node(value, map),
        MiddleNodeType::ListLiteral(MirList {
            data_type: _,
            values,
        })
        | MiddleNodeType::ScopeDeclaration(MirScopeDecl { body: values, .. }) => {
            for v in values {
                inline_in_node(v, map);
            }
        }
        MiddleNodeType::LoopDeclaration(MirLoop { state, body, .. }) => {
            if let Some(s) = state.as_mut() {
                inline_in_node(s, map);
            }
            inline_in_node(body, map);
        }
        _ => {}
    }
}
