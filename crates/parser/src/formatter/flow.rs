use crate::{
    ast::{
        formatter::Formatter,
        nodes::flow::{AstBreak, AstContinue, AstDefer, AstEmit, AstReturn, AstTry, TryCatch},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstEmit {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        match self {
            AstEmit::Scope(x) => format!("emit {}", x.format(formatter)),
            AstEmit::Channel { channel, value } => {
                format!(
                    "emit {} {}",
                    channel.format(formatter),
                    value.format(formatter)
                )
            }
        }
    }
}

impl AstFormatting for AstBreak {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = String::from("break");

        if let Some(label) = &self.label {
            txt.push_str(&format!(" @{}", label));
        }

        if let Some(value) = &self.value {
            txt.push(' ');
            txt.push_str(&value.format(formatter));
        }

        txt
    }
}

impl AstFormatting for AstContinue {
    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        let mut txt = String::from("continue");

        if let Some(label) = &self.label {
            txt.push_str(&format!(" @{}", label));
        }

        txt
    }
}

impl AstFormatting for AstReturn {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        match &self.value {
            Some(value) => format!("return {}", value.format(formatter)),
            _ => String::from("return"),
        }
    }
}

impl AstFormatting for AstDefer {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "defer {}{}",
            if self.function { "return " } else { "" },
            self.value.format(formatter)
        )
    }
}

impl AstFormatting for TryCatch {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = String::new();

        if let Some(name) = &self.name {
            txt.push_str(&format!(" : {}", name));
        }

        txt.push_str(&format!(" {}", self.body.format(formatter)));
        txt
    }
}

impl AstFormatting for AstTry {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("try {}", self.value.format(formatter));

        if let Some(catch) = &self.catch {
            txt.push_str(&catch.format(formatter));
        }

        txt
    }
}
