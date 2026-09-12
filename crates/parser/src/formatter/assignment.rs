use crate::{
    ast::{
        formatter::Formatter,
        nodes::{
            AstNodeType,
            assignment::{AstAssignDestructure, AstAssignment},
            binary::{AstBinary, AstBoolean},
        },
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstAssignment {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        match &self.value.node_type {
            AstNodeType::BinaryExpression(AstBinary {
                left,
                right,
                operator,
            }) if left.node_type == self.identifier.node_type => format!(
                "{} {}= {}",
                self.identifier.format(formatter),
                operator,
                right.format(formatter)
            ),
            AstNodeType::BooleanExpression(AstBoolean {
                left,
                right,
                operator,
            }) if left.node_type == self.identifier.node_type => format!(
                "{} {}= {}",
                self.identifier.format(formatter),
                operator,
                right.format(formatter)
            ),
            _ => {
                let lhs = self.identifier.format(formatter);
                let rhs = self.value.format(formatter);
                format!("{} := {}", lhs, rhs)
            }
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        match &self.value.node_type {
            AstNodeType::BinaryExpression(AstBinary { left, .. })
            | AstNodeType::BooleanExpression(AstBoolean { left, .. })
                if left.node_type == self.identifier.node_type =>
            {
                None
            }
            _ => {
                let lhs = self.identifier.format(formatter);
                let rhs = self.value.format(formatter);
                Some(format!(
                    "{} :=\n{}",
                    lhs,
                    formatter.fmt_txt_with_tab(&rhs, 1, true)
                ))
            }
        }
    }
}

impl AstFormatting for AstAssignDestructure {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{} := {}",
            self.pattern.format(formatter, false),
            self.value.format(formatter)
        )
    }
}
