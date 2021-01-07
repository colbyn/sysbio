#![allow(unused)]
pub mod bel;
pub mod ingest;


pub fn reload(path: &str) -> Vec<bel_format::parser::Ast> {
    let source = std::fs::read_to_string(path).unwrap();
    let (terms, reports) = bel_format::from_str(&source);
    if false {
        println!("");
        for report in reports {
            println!("{}", report.line);
        }
    }
    terms
}

fn main() {
    // let source_file = "data/source/dataset-alhpa.txt";
    // // let source_file = "source.txt";
    // // let source_file = "data/source/hetionet-v1.0.bel";
    
    let xs = reload("data/source/dataset-alhpa.txt");
    let ys = reload("data/source/hetionet-v1.0.bel");
    let zs = vec![xs, ys]
        .concat()
        .into_iter()
        .map(|x| format!("{}", x))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::write("dataset.txt", zs).unwrap();
}
