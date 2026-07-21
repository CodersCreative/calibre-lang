use crate::{ast::MiddleNode, errors::MiddleErr, tags::context::PackageMetadata};
use calibre_parser::{Location, Span};

#[derive(Debug, Clone, Default)]
pub struct MiddleContext {
    pub current_location: Option<Location>,
    pub errors: Vec<MiddleErr>,
    pub stdlib_nodes: Vec<MiddleNode>,
    pub package_metadata: Option<PackageMetadata>,
}

impl MiddleContext {
    pub fn push_error(&mut self, err: MiddleErr) {
        if !self.errors.contains(&err) {
            self.errors.push(err);
        }
    }

    pub fn take_errors(&mut self) -> Vec<MiddleErr> {
        std::mem::take(&mut self.errors)
    }

    pub fn current_span(&self) -> Span {
        self.current_location
            .as_ref()
            .map(|loc| loc.span)
            .unwrap_or_default()
    }

    pub fn err_at_current(&self, err: MiddleErr) -> MiddleErr {
        if let Some(location) = &self.current_location {
            MiddleErr::At(location.span, Box::new(err))
        } else {
            err
        }
    }
}
