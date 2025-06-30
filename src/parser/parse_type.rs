use crate::{checker::AddressingMode, lexer::Token};

#[derive(Debug, Clone)]
pub enum InnerType {
    Primitive { ident: Token },
    Nested { inner: Box<ParseType> },
}

#[derive(Debug, Clone)]
pub struct ParseType {
    pub inner_type: InnerType,
    pub addr_mode: AddressingMode,
}

impl ParseType {
    pub fn get_ident(&self) -> &Token {
        match &self.inner_type {
            InnerType::Primitive { ident } => ident,
            InnerType::Nested { inner, .. } => inner.get_ident(),
        }
    }
}
