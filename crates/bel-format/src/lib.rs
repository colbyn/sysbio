#![allow(unused)]
pub(crate) mod parser_utils;
pub mod parser;

pub fn from_str(source: &str) -> (Vec<parser::Ast>, Vec<parser::ParserError>) {
    parser::parse_lines(source)
}

/// Try to parse the given value form a `&str`, or
/// fail with an error message printed to stdout.
pub fn from_str_or_panic(source: &str) -> Vec<parser::Ast> {
    let (ast, errors) = parser::parse_lines(source);
    let has_errors = !errors.is_empty();
    for error in errors {
            match error {
                parser::ParserError::Unparsed(rest) => {
                    panic!("[crate bel-format] unparsed line: {:?}", rest);
                }
                parser::ParserError::ParserError(msg) => {
                    panic!("[crate bel-format] parser error:");
                    eprintln!("{}\n", msg);
                }
            }
    }
    if has_errors {
        panic!("[crate bel-format] parser failed");
    }
    ast
}

