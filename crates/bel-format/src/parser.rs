use std::rc::Rc;
use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n},
    combinator::map_res,
    sequence::tuple,
    sequence::delimited,
    character::complete::char,
    bytes::complete::is_not,
    error::ParseError,
    character::complete::multispace0,
    combinator::recognize,
    sequence::pair,
    branch::alt,
    character::complete::{alpha1},
    character::complete::alphanumeric1,
    combinator::{cut, map, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    character::complete::{digit1, multispace1, one_of},
    multi::separated_list1,
    Parser,
};

///////////////////////////////////////////////////////////////////////////////
// PARSER DATA TYPES
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct NsArg {
    pub prefix: String,
    pub value: String,
    pub label: Option<String>,
}

/// # Parser AST
/// 
/// This AST implementation is extremely simple and handles differences between
/// BEL versions (or whatever is causing the inconsistencies that I’m seeing).
/// 
/// At some point, I may introduce a higher level AST; akin to compiler
/// pipelines. 
#[derive(Debug, Clone)]
pub enum Ast {
    NsArg(NsArg),
    Symbol(String),
    Function(String, Vec<Ast>),
    Relation(Box<Ast>, String, Box<Ast>),
    /// Some functions contain a single string argument for some reason.
    String(String),
}

impl std::fmt::Display for NsArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = self.label.as_ref() {
            write!(f, "{}:{} ! {}", self.prefix, self.value, label)
        } else {
            write!(f, "{}:{}", self.prefix, self.value)
        }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::NsArg(ns) => {
                write!(f, "{}", ns)
            }
            Ast::Symbol(sym) => {
                write!(f, "{}", sym)
            }
            Ast::Function(name, args) => {
                let args = args
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", name, args)
            }
            Ast::Relation(left, infix_op, right) => {
                write!(f, "{} {} {}", left, infix_op, right)
            }
            Ast::String(string) => {
                write!(f, "{:?}", string)
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
// INTERNAL PARSER UTILS
///////////////////////////////////////////////////////////////////////////////

fn parens<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where F: Fn(&'a str) -> IResult<&'a str, O, E>
{
    delimited(char('('), inner, char(')'))
}

fn identifier(source: &str) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>> {
    let (source, ident) = recognize(
        pair(
            alt((alphanumeric1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(source)?;
    Ok((source, ident.to_owned()))
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where F: Fn(&'a str) -> IResult<&'a str, O, E>
{
  delimited(
    multispace0,
    inner,
    multispace0,
  )
}

fn parse_ns_arg(
    source: &str
) -> Result<(&str, NsArg), nom::Err<nom::error::Error<&str>>>
{
    use crate::parser_utils::string::parse_string;
    fn parse_text(
        source: &str
    ) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>> {
        alt((parse_string, identifier))(source)
    }
    fn parse_ident(
        source: &str,
    ) -> Result<(&str, (String, String)), nom::Err<nom::error::Error<&str>>> {
        let (source, prefix) = parse_text(source)?;
        let (source, _) = tag(":")(source)?;
        let (source, value) = parse_text(source)?;
        Ok((source, (prefix, value)))
    }
    fn parse_label(
        source: &str
    ) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>> {
        let (source, _) = ws(tag("!"))(source)?;
        let (source, value) = parse_text(source)?;
        Ok((source, value))
    }
    fn default_parser(
        source: &str
    ) -> Result<(&str, NsArg), nom::Err<nom::error::Error<&str>>> {
        let (source, (prefix, value)) = parse_ident(source)?;
        let (source, label) = opt(parse_label)(source)?;
        let ast = NsArg{prefix, value, label};
        Ok((source, ast))
    }
    default_parser(source)
}

fn parse_ns(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>>
{
    let (rest, ns_arg) = parse_ns_arg(source)?;
    Ok((rest, Ast::NsArg(ns_arg)))
}

fn parse_symbol_to_ast(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>>
{
    let single = alt((
        tag("A"),
        tag("R"),
        tag("N"),
        tag("D"),
        tag("C"),
        tag("E"),
        tag("Q"),
        tag("G"),
        tag("H"),
        tag("I"),
        tag("L"),
        tag("K"),
        tag("M"),
        tag("F"),
        tag("P"),
        tag("S"),
        tag("T"),
        tag("W"),
        tag("Y"),
        tag("V"),
        tag("U"),
    ));
    let long = alt((
        tag("Ala"),
        tag("Arg"),
        tag("Asn"),
        tag("Asp"),
        tag("Cys"),
        tag("Glu"),
        tag("Gln"),
        tag("Gly"),
        tag("His"),
        tag("Ile"),
        tag("Leu"),
        tag("Lys"),
        tag("Met"),
        tag("Phe"),
        tag("Pro"),
        tag("Ser"),
        tag("Thr"),
        tag("Trp"),
        tag("Tyr"),
        tag("Val"),
    ));
    let (rest, sym) = alt((single, long, digit1))(source)?;
    Ok((rest, Ast::Symbol(sym.to_owned())))
}

fn parse_string_to_ast(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>>
{
    use crate::parser_utils::string::parse_string;
    let (rest, value) = parse_string(source)?;
    Ok((rest, Ast::String(value)))
}

fn parse_function(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>>
{
    fn term_parser(
        source: &str
    ) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
        alt((
            parse_arg_relation,
            parse_ns,
            parse_function,
            parse_symbol_to_ast,
            parse_string_to_ast,
        ))(source)
    }
    fn function_arguments(
        source: &str
    ) -> Result<(&str, Vec<Ast>), nom::Err<nom::error::Error<&str>>> {
        let (source, _) = tag("(")(source)?;
        let (source, args) = separated_list1(ws(char(',')), ws(term_parser))(source)?;
        let (source, _) = tag(")")(source)?;
        Ok((source, args))
    }
    let (source, name) = ws(identifier)(source)?;
    let (source, args) = ws(function_arguments)(source)?;
    let ast = Ast::Function(
        name,
        args,
    );
    Ok((source, ast))
}

fn top_level_parser(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
    parse_relation(source, true)
}

fn parse_arg_relation(
    source: &str,
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
    parse_relation(source, false)
}

fn parse_relation(
    source: &str,
    top_level: bool,
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
    fn term_parser(
        source: &str,
    ) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
        alt((parse_function, parse_ns))(source)
    }
    let (source, left) = ws(term_parser)(source)?;
    let (source, relation) = ws(identifier)(source)?;
    let (source, right) = ws(term_parser)(source)?;
    let ast = Ast::Relation(
        Box::new(left),
        relation,
        Box::new(right),
    );
    Ok((source, ast))
}

///////////////////////////////////////////////////////////////////////////////
// PARSER ENTRYPOINT
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum ParserError {
    Unparsed(String),
    ParserError(String),
}

#[derive(Debug, Clone)]
pub struct ErrorReport {
    pub error: ParserError,
    pub line: String,
}

impl std::fmt::Display for ErrorReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lines = vec![
            format!("[crate bel-format] given line: {}", &self.line),
            match &self.error {
                ParserError::Unparsed(rest) => {
                    format!("\t☞ [unparsed input]: {:?}\n", rest)
                }
                ParserError::ParserError(msg) => {
                    format!("\t☞ [parser error]: {}\n", msg)
                }
            }
        ];
        write!(f, "{}", lines.join("\n"))
    }
}


/// `log_errors` will print error messgaes to stdout while parsing.
pub(crate) fn parse_lines(
    source: &str,
    log_errors: bool,
) -> (Vec<Ast>, Vec<ErrorReport>) {
    let (ast, errors) = source
        .lines()
        .filter(|line| {
            !line.starts_with("#")
        })
        .filter(|line| {
            !line.starts_with("DEFINE")
        })
        .filter(|line| {
            !line.starts_with("SET")
        })
        .filter(|line| {
            !line.starts_with("UNSET")
        })
        .filter(|line| {
            !line.trim().is_empty()
        })
        .map(|line| match top_level_parser(line) {
            Ok((rest, xs)) if !rest.is_empty() => {
                let error = ParserError::Unparsed(rest.to_owned());
                let report = ErrorReport {
                    error,
                    line: line.to_owned(),
                };
                if log_errors {
                    eprintln!("{}", report);
                }
                (Some(xs), Some(report))
            }
            Ok((_, xs)) => (Some(xs), None),
            Err(msg) => {
                let error = ParserError::ParserError(format!("{}", msg));
                let report = ErrorReport {
                    error,
                    line: line.to_owned(),
                };
                if log_errors {
                    eprintln!("{}", report);
                }
                (None, Some(report))
            }
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let ast = ast
        .into_iter()
        .filter_map(|x| x)
        .collect::<Vec<_>>();
    let errors = errors
        .into_iter()
        .filter_map(|x| x)
        .collect::<Vec<_>>();
    (ast, errors)
}