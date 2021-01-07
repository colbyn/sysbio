//! Biological Expression Language (BEL) - POC AST & Parser
#![allow(unused)]
pub(crate) mod parser_utils;
pub mod parser;

pub fn from_str(source: &str) -> (Vec<parser::Ast>, Vec<parser::ErrorReport>) {
    parser::parse_lines(source, false)
}

/// Try to parse the given value form a `&str`, or
/// fail with an error message printed to stdout.
pub fn from_str_or_panic(source: &str) -> Vec<parser::Ast> {
    let (result, errors) = parser::parse_lines(source, true);
    if !errors.is_empty() {
        panic!("[crate bel-format] parser failed");
    }
    result
}

