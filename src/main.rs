#![allow(unused)]
pub mod bel;
pub mod ingest;


fn main() {
    // let source_file = "data/source/dataset-alhpa.txt";
    // let source_file = "source.txt";
    let source_file = "data/source/hetionet-v1.0.bel";
    let source = std::fs::read_to_string(source_file).unwrap();
    let (terms, reports) = bel_format::from_str(&source);
    println!("");
    for report in reports {
        println!("{}", report.line);
    }
    println!("DONE");
}
