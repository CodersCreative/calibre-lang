use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
};
use calibre_parser::ast::{
    idents::ParserText,
    nodes::{AstNode, AstNodeType},
};
use std::{fmt::Debug, sync::Arc};
use ustr::{Ustr, UstrMap};
use wasm_sync::Mutex;

mod builders;
pub mod context;
pub mod defaults;

pub type TagHandlerFn = Arc<
    Mutex<
        dyn Fn(
                &mut MiddleEnvironment,
                ScopeId,
                AstNode,
                ParserText,
                Vec<AstNode>,
            ) -> Result<MiddleNode, MiddleErr>
            + Send
            + Sync,
    >,
>;

#[derive(Clone)]
pub struct TagHandler {
    pub handler: TagHandlerFn,
}

impl Debug for TagHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TagHandler")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TagInfo {
    Init(i32),
    Fin(i32),
    Default,
    Builder,
    Panics,
    Bench,
    CallerContext,
    IgnoreInvalidReturn,
    IgnoreInvalidLet,
    IgnoreInvalidTypeCheck,
    Suite(Ustr),
    Todo(Option<Ustr>),
    Deprecated(Option<Ustr>),
    Skip(Option<Ustr>),
}

#[derive(Debug, Clone, Default)]
pub struct Tagging {
    pub tag_handlers: UstrMap<TagHandler>,
    pub init_functions: Vec<(i32, Ustr)>,
    pub fin_functions: Vec<(i32, Ustr)>,
    pub tag_info: Vec<TagInfo>,
    pub caller_context: UstrMap<Ustr>,
}

impl MiddleEnvironment {
    pub fn register_tag_handlers(&mut self) {
        let init_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             args: Vec<AstNode>| {
                let priority = if let Some(AstNodeType::IntLiteral(val)) =
                    args.first().map(|x| &x.node_type)
                {
                    val.parse::<i32>().unwrap_or(100)
                } else {
                    100
                };

                env.tagging.tag_info.push(TagInfo::Init(priority));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();

                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("init"),
            TagHandler {
                handler: init_handler,
            },
        );

        // TODO
        let backend_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             args: Vec<AstNode>| {
                #[cfg(feature = "native")]
                let backend = "interpreter-native";
                #[cfg(not(feature = "native"))]
                let backend = "interpreter-wasm";
                let mut build = false;

                for arg in args {
                    if let AstNodeType::StringLiteral(val) = arg.node_type
                        && val.text == backend
                    {
                        build = true;
                        break;
                    }
                }

                if build {
                    env.evaluate_inner(scope, node)
                } else {
                    Ok(MiddleNode::new(MiddleNodeType::EmptyLine, node.span))
                }
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("backend"),
            TagHandler {
                handler: backend_handler,
            },
        );

        let os_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             args: Vec<AstNode>| {
                let os = std::env::consts::OS;
                let mut build = false;

                for arg in args {
                    if let AstNodeType::StringLiteral(val) = arg.node_type
                        && val.text == os
                    {
                        build = true;
                        break;
                    }
                }

                if build {
                    env.evaluate_inner(scope, node)
                } else {
                    Ok(MiddleNode::new(MiddleNodeType::EmptyLine, node.span))
                }
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("os"),
            TagHandler {
                handler: os_handler,
            },
        );

        let fin_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             args: Vec<AstNode>| {
                let priority = if let Some(AstNodeType::IntLiteral(val)) =
                    args.first().map(|x| &x.node_type)
                {
                    val.parse::<i32>().unwrap_or(100)
                } else {
                    100
                };

                env.tagging.tag_info.push(TagInfo::Fin(priority));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();

                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("fin"),
            TagHandler {
                handler: fin_handler,
            },
        );

        let default_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Default);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("default"),
            TagHandler {
                handler: default_handler,
            },
        );

        let builder_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Builder);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("builder"),
            TagHandler {
                handler: builder_handler,
            },
        );

        let panics_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Panics);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("panics"),
            TagHandler {
                handler: panics_handler,
            },
        );

        let todo_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             mut args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Todo(args.pop().and_then(
                    |x| match x.node_type {
                        AstNodeType::StringLiteral(x) => Some(Ustr::from(&x.text)),
                        _ => None,
                    },
                )));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("todo"),
            TagHandler {
                handler: todo_handler,
            },
        );

        let deprecated_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             mut args: Vec<AstNode>| {
                env.tagging
                    .tag_info
                    .push(TagInfo::Deprecated(args.pop().and_then(
                        |x| match &x.node_type {
                            AstNodeType::StringLiteral(x) => Some(Ustr::from(&x.text)),
                            _ => None,
                        },
                    )));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("deprecated"),
            TagHandler {
                handler: deprecated_handler,
            },
        );

        let skip_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             mut args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Skip(args.pop().and_then(
                    |x| match &x.node_type {
                        AstNodeType::StringLiteral(x) => Some(Ustr::from(&x.text)),
                        _ => None,
                    },
                )));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("skip"),
            TagHandler {
                handler: skip_handler,
            },
        );

        let bench_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Bench);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("bench"),
            TagHandler {
                handler: bench_handler,
            },
        );

        let suite_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             mut args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::Suite(
                    args.pop()
                        .map(|x| match &x.node_type {
                            AstNodeType::StringLiteral(x) => Ustr::from(&x.text),
                            _ => Ustr::default(),
                        })
                        .unwrap_or_default(),
                ));
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("suite"),
            TagHandler {
                handler: suite_handler,
            },
        );

        let package_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                let middle = env.evaluate_with_package_injection(scope, node)?;
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("package"),
            TagHandler {
                handler: package_handler,
            },
        );

        let caller_context_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::CallerContext);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("caller_context"),
            TagHandler {
                handler: caller_context_handler,
            },
        );

        let ignore_invalid_return_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::IgnoreInvalidReturn);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("ignore_invalid_return"),
            TagHandler {
                handler: ignore_invalid_return_handler,
            },
        );

        let ignore_invalid_let_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::IgnoreInvalidLet);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("ignore_invalid_let"),
            TagHandler {
                handler: ignore_invalid_let_handler,
            },
        );

        let ignore_invalid_type_check: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                env.tagging.tag_info.push(TagInfo::IgnoreInvalidTypeCheck);
                let middle = env.evaluate_inner(scope, node)?;
                let _ = env.tagging.tag_info.pop();
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("ignore_invalid_type_check"),
            TagHandler {
                handler: ignore_invalid_type_check,
            },
        );

        let current_context_handler: TagHandlerFn = Arc::new(Mutex::new(
            |env: &mut MiddleEnvironment,
             scope: ScopeId,
             node: AstNode,
             _tag: ParserText,
             _args: Vec<AstNode>| {
                let middle = env.evaluate_with_current_context_injection(scope, node)?;
                Ok(middle)
            },
        ));

        self.tagging.tag_handlers.insert(
            Ustr::from("current_context"),
            TagHandler {
                handler: current_context_handler,
            },
        );
    }
}
