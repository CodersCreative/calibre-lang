use std::fmt::Display;

use crate::{
    ast::MiddleNode, errors::MiddleErr, symbols::resolve::Key, tags::context::PackageMetadata,
};
use calibre_parser::{Location, Span};
use rustc_hash::FxHashMap;
use ustr::Ustr;

#[derive(Debug, Clone, Default)]
pub struct MiddleContext {
    pub ustr_to_key: FxHashMap<Ustr, Key>,
    pub current_location: Option<Location>,
    pub errors: Vec<MiddleErr>,
    pub stdlib_nodes: Vec<MiddleNode>,
    pub package_metadata: Option<PackageMetadata>,
    pub type_check: bool,
    pub in_stdlib: Option<Ustr>,
    pub in_temp_scope: bool,
    pub in_generator: bool,
    pub counter: usize,
}

impl MiddleContext {
    pub fn push_error(&mut self, err: MiddleErr) {
        if !self.errors.contains(&err) {
            self.errors.push(err);
        }
    }

    pub fn with_type_check(mut self, type_check: bool) -> Self {
        self.type_check = type_check;
        self
    }

    pub fn take_errors(&mut self) -> Vec<MiddleErr> {
        std::mem::take(&mut self.errors)
    }

    #[inline]
    pub fn current_span(&self) -> Span {
        self.current_location
            .as_ref()
            .map(|loc| loc.span)
            .unwrap_or_default()
    }

    pub fn increment_counter(&mut self) -> usize {
        let value = self.counter;
        self.counter += 1;
        value
    }

    pub fn err_at_current(&self, err: MiddleErr) -> MiddleErr {
        self.err_at_span(Span::default(), err)
    }

    pub fn err_at_span(&self, span: Span, err: MiddleErr) -> MiddleErr {
        if span.is_none() {
            if let Some(location) = &self.current_location {
                MiddleErr::At(location.span, Box::new(err))
            } else {
                err
            }
        } else {
            MiddleErr::At(span, Box::new(err))
        }
    }

    pub fn get_temp(&mut self, name: impl Display) -> String {
        format!("mir_tmp_{}_{}", name, self.increment_counter())
    }

    pub fn convert_key_to_ustr(&mut self, key: Key) -> Ustr {
        let temp_ident = Ustr::from(&format!("#{}", self.increment_counter()));
        self.ustr_to_key.insert(temp_ident, key);
        temp_ident
    }

    pub fn convert_ustr_to_key(&self, name: &Ustr) -> Option<&Key> {
        self.ustr_to_key.get(name)
    }
}
