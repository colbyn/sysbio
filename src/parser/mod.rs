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

// fn parens(input: &str, ) -> IResult<&str, &str> {
//     delimited(char('('), is_not(")"), char(')'))(input)
// }

fn parens<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where F: Fn(&'a str) -> IResult<&'a str, O, E>
{
  delimited(
    char('('),
    inner,
    char(')'),
  )
}

pub fn identifier(source: &str) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>> {
    let (source, ident) = recognize(
        pair(
        alt((alphanumeric1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"))))
        )
    )(source)?;
    Ok((source, ident.to_owned()))
}

fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn hex_primary(input: &str) -> IResult<&str, u8> {
    map_res(
        take_while_m_n(2, 2, is_hex_digit),
        from_hex
    )(input)
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where F: Fn(&'a str) -> IResult<&'a str, O, E>
{
  delimited(
    multispace0,
    inner,
    multispace0
  )
}

#[derive(Debug, Clone)]
pub struct NsArg {
    prefix: String,
    value: String,
    label: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Ast {
    NsArg(NsArg),
    Symbol(String),
    Function(String, Vec<Ast>),
    Relation(Box<Ast>, String, Box<Ast>),
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

fn parse_function(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>>
{
    fn parse_ns(
        source: &str
    ) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
        let (source, ns) = parse_ns_arg(source)?;
        Ok((source, Ast::NsArg(ns)))
    }
    fn ns_or_fun_call(
        source: &str
    ) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
        alt((parse_ns, parse_function, parse_symbol_to_ast))(source)
    }
    fn args_between_comma(
        source: &str
    ) -> Result<(&str, Vec<Ast>), nom::Err<nom::error::Error<&str>>> {
        separated_list1(tag(","), ns_or_fun_call)(source)
    }
    fn arg_parser(
        source: &str
    ) -> Result<(&str, Vec<Ast>), nom::Err<nom::error::Error<&str>>> {
        let (source, args) = parens(args_between_comma)(source)?;
        Ok((source, args))
    }
    let (source, name) = ws(identifier)(source)?;
    let (source, mut args) = ws(arg_parser)(source)?;
    let ast = Ast::Function(
        name,
        args,
    );
    Ok((source, ast))
}

fn term_parser(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
    // alt((parse_function, parse_relation))(source)
    parse_function(source)
}

fn parse_relation(
    source: &str
) -> Result<(&str, Ast), nom::Err<nom::error::Error<&str>>> {
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

fn parse_lines(
    source: &str
) -> Vec<Ast> {
    let lines = source
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
        .map(|line| match parse_function(line) {
            Ok((rest, _)) if !rest.is_empty() => {
                eprintln!("unparsed: {:?}", rest);
                panic!()
            }
            Ok((_, x)) => x,
            Err(msg) => {
                eprintln!("FAILED: {}\n", line);
                eprintln!("{}", msg);
                panic!()
            }
        })
        .collect::<Vec<_>>();
    lines
}

pub fn main() {
    let source_file = "data/source/hetionet-v1.0.bel";
    let source_file = "data/source/dataset-alhpa.txt";
    let source_file = "source.txt";
    let source = String::from_utf8(
        std::fs::read(source_file).unwrap()
    ).unwrap();

    let result = match parse_relation(&source) {
        Ok(x) => x,
        Err(msg) => {
            eprintln!("{}", msg);
            panic!()
        }
    };
    println!("{:#?}", result);
    
    // let result = parse_lines(&source);
    // // for line in result {
    // //     println!("{:?}", line);
    // // }
    // println!("DONE");
}