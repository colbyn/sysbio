#[derive(Debug, Clone)]
pub enum Argument {
    Function(Function),
    NsArg(Namespace),
    StrArg(String),
}

impl Argument {
    pub fn ns(prefix: &str, value: &str) -> Self {
        Argument::NsArg(Namespace {
            prefix: Some(prefix.to_owned()),
            value: value.to_owned(),
            species_id: None,
        })
    }
    pub fn fun(fun: Function) -> Self {
        Argument::Function(fun)
    }
}

#[derive(Debug, Clone)]
pub struct Namespace {
    /// Namespace prefix (HGNC for Human Gene Nomenclature Committee)
    prefix: Option<String>,
    /// Namespace value (AKT1)
    value: String,
    /// Species ID if relevant to the term (TAX:9606 for human)
    species_id: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Function {
    /// Chemical or drug abundance
    /// * `abundance [a]`
    /// * `abundance(NSArg, loc()?)`
    Abundance(Vec<Argument>),

    /// denotes activity of protein, RNA, complex
    /// * `activity [act]`
    /// * `activity(g|r|m|p|complex(), ma()?)`
    Activity(Vec<Argument>),

    /// biological processes
    /// * `biologicalProcess [bp]`
    /// * `biologicalProcess(NSArg)`
    BiologicalProcess(Vec<Argument>),

    /// denote secretion of abundance from the cell
    /// * `cellSecretion [sec]`
    /// * `cellSecretion(p|complex|a())`
    CellSecretion(Vec<Argument>),

    /// expresses an abundance moved to the cellular surface
    /// * `cellSurfaceExpression [surf]`
    /// * `cellSurfaceExpression(p|complex|a())`
    CellSurfaceExpression(Vec<Argument>),

    /// Complex of biological/chemical abundances
    /// * `complexAbundance [complex]`
    /// * `complexAbundance(a|p|g|m|r|pop(), NSArg, loc()?)`
    ComplexAbundance(Vec<Argument>),

    /// Not a complex but a set of abundances that together produce an effect
    /// * `compositeAbundance [composite]`
    /// * `compositeAbundance(a|act|p|g|m|r|complex|pop(), NSArg)`
    CompositeAbundance(Vec<Argument>),

    /// denotes degradation of an abundance
    /// * `degradation [deg]`
    Degradation(Vec<Argument>),

    /// modifier function for protein fragments
    /// * `fragment [frag]`
    /// * `fragment(StrArg, StrArg)`
    Fragment(Vec<Argument>),

    /// modifier function for biomolecule fusions
    /// * `fusion [fus]`
    /// * `fusion(NSArg, StrArg, NSArg, StrArg)`
    Fusion(Vec<Argument>),

    /// Gene abundance
    /// * `geneAbundance [g]`
    /// * `geneAbundance(NSArg, loc()?, var()*)` or `geneAbundance(fus(), loc()?, var()*)`
    GeneAbundance(Vec<Argument>),

    /// modifier function for abundance location
    /// * `location [loc]`
    Location(Vec<Argument>),

    /// microRNA abundance
    /// * `microRNAAbundance [m]`
    /// * `microRNAAbundance(NSArg, loc()?, var()*)` or `microRNAAbundance(fus(), loc()?, var()*)`
    MicroRNAAbundance(Vec<Argument>),

    /// modifier function – specifies specific type of activity
    /// * `molecularActivity [ma]`
    /// * `molecularActivity(StrArgNSArg)`
    MolecularActivity(Vec<Argument>),

    /// pathologies - disease or pathological process
    /// * `pathology [path]`
    /// * `pathology(NSArg)`
    Pathology(Vec<Argument>),

    /// population abundance
    /// * `populationAbundance [pop]`
    /// * `populationAbundance(NSArg, loc()?)`
    PopulationAbundance(Vec<Argument>),

    /// protein abundance
    /// * `proteinAbundance [p]`
    /// * `proteinAbundance(NSArg, loc|frag()?, var|pmod()*)` or `proteinAbundance(fus(), loc()?, var()*)`
    ProteinAbundance(Vec<Argument>),

    /// modifier function for protein modifications
    /// * `proteinModification [pmod]`
    /// * `proteinModification(StrArgNSArg, StrArgNSArg?, StrArg?)`
    ProteinModification(Vec<Argument>),

    /// reaction function - reactants and products
    /// * `reaction [rxn]`
    /// * `reaction(reactants(), products())`
    Reaction(Vec<Argument>),

    /// RNA abundance
    /// * `rnaAbundance [r]`
    /// * `rnaAbundance(NSArg, loc()?, var()*)` or `rnaAbundance(fus(), loc()?, var()*)`
    RnaAbundance(Vec<Argument>),

    /// denotes movement of abundance from one location to another
    /// * `translocation [tloc]`
    /// * `translocation(g|p|r|m|complex|pop|a(), fromLoc(), toLoc())`
    Translocation(Vec<Argument>),

    /// modifier function for gene/RNA/protein variants
    /// * `variant [var]`
    /// * `variant(StrArg)`
    Variant(Vec<Argument>),
}

impl Function {
    /// `abundance [a]`
    pub fn a1(arg: Argument) -> Self {Function::Abundance(vec![arg])}
    /// `activity [act]`
    pub fn act1(arg: Argument) -> Self {Function::Activity(vec![arg])}
    /// `activity [act]`
    pub fn act2(arg1: Argument, arg2: Argument) -> Self {Function::Activity(vec![arg1, arg2])}
    /// `biologicalProcess [bp]`
    pub fn bp1(arg: Argument) -> Self {Function::BiologicalProcess(vec![arg])}
    /// `cellSecretion [sec]`
    pub fn sec1(arg: Argument) -> Self {Function::CellSecretion(vec![arg])}
    /// `cellSurfaceExpression [surf]`
    pub fn surf1(arg: Argument) -> Self {Function::CellSurfaceExpression(vec![arg])}
    /// `complexAbundance [complex]`
    pub fn complex1(arg: Argument) -> Self {Function::ComplexAbundance(vec![arg])}
    /// `compositeAbundance [composite]`
    pub fn composite1(arg: Argument) -> Self {Function::CompositeAbundance(vec![arg])}
    /// `degradation [deg]`
    pub fn deg1(arg: Argument) -> Self {Function::Degradation(vec![arg])}
    /// `fragment [frag]`
    pub fn frag1(arg: Argument) -> Self {Function::Fragment(vec![arg])}
    /// `fusion [fus]`
    pub fn fus1(arg: Argument) -> Self {Function::Fusion(vec![arg])}
    /// `geneAbundance [g]`
    pub fn g1(arg: Argument) -> Self {Function::GeneAbundance(vec![arg])}
    /// `location [loc]`
    pub fn loc1(arg: Argument) -> Self {Function::Location(vec![arg])}
    /// `microRNAAbundance [m]`
    pub fn m1(arg: Argument) -> Self {Function::MicroRNAAbundance(vec![arg])}
    /// `molecularActivity [ma]`
    pub fn ma1(arg: Argument) -> Self {Function::MolecularActivity(vec![arg])}
    /// `pathology [path]`
    pub fn path1(arg: Argument) -> Self {Function::Pathology(vec![arg])}
    /// `populationAbundance [pop]`
    pub fn pop1(arg: Argument) -> Self {Function::PopulationAbundance(vec![arg])}
    /// `proteinAbundance [p]`
    pub fn p1(arg: Argument) -> Self {Function::ProteinAbundance(vec![arg])}
    /// `proteinModification [pmod]`
    pub fn pmod1(arg: Argument) -> Self {Function::ProteinModification(vec![arg])}
    /// `reaction [rxn]`
    pub fn rxn1(arg: Argument) -> Self {Function::Reaction(vec![arg])}
    /// `rnaAbundance [r]`
    pub fn r1(arg: Argument) -> Self {Function::RnaAbundance(vec![arg])}
    /// `variant [var]`
    pub fn var1(arg: Argument) -> Self {Function::Variant(vec![arg])}
}


#[derive(Debug, Clone)]
pub enum Relation {
    /// Deprecated – A is analogous to B
    /// * `analogous`
    Analogous(Vec<Argument>),

    /// A is associated with B - least informative relationship
    /// * `association [--]`
    Association(Vec<Argument>),

    /// Deprecated – A is a biomarker for B
    /// * `biomarkerFor`
    BiomarkerFor(Vec<Argument>),

    /// A causes no change in B
    /// * `causesNoChange [cnc]`
    CausesNoChange(Vec<Argument>),

    /// A indirectly decreases B
    /// * `decreases [-|]`
    Decreases(Vec<Argument>),

    /// A directly decreases B
    /// * `directlyDecreases [=|]`
    DirectlyDecreases(Vec<Argument>),

    /// A directly increases B
    /// * `directlyIncreases [=>]`
    DirectlyIncreases(Vec<Argument>),

    /// A has activity B, e.g. kinase activity
    /// * `hasActivity`
    HasActivity(Vec<Argument>),

    /// A has component B (for complexes)
    /// * `hasComponent`
    HasComponent(Vec<Argument>),

    /// A has components list(B, C, D, …)
    /// * `hasComponents`
    HasComponents(Vec<Argument>),

    /// A has a member B
    /// * `hasMember`
    HasMember(Vec<Argument>),

    /// A has members list(B, C, D, …)
    /// * `hasMembers`
    HasMembers(Vec<Argument>),

    /// A indirectly increases B
    /// * `increases [->]`
    Increases(Vec<Argument>),

    /// A is a subset of B
    /// * `isA`
    IsA(Vec<Argument>),

    /// A is negatively correlated with B
    /// * `negativeCorrelation [neg]`
    NegativeCorrelation(Vec<Argument>),

    /// A is orthologous to B
    /// * `orthologous`
    Orthologous(Vec<Argument>),

    /// A is positively correlated with B
    /// * `positiveCorrelation [pos]`
    PositiveCorrelation(Vec<Argument>),

    /// Deprecated – A is a prognostic biomarker for B
    /// * `prognosticBiomarkerFor`
    PrognosticBiomarkerFor(Vec<Argument>),

    /// A is a rate limiting step of B
    /// * `rateLimitingStepOf`
    RateLimitingStepOf(Vec<Argument>),

    /// A regulates (effects) B somehow
    /// * `regulates [reg]`
    Regulates(Vec<Argument>),

    /// A is a subprocess of B
    /// * `subProcessOf`
    SubProcessOf(Vec<Argument>),

    /// gene is transcribed to RNA
    /// * `transcribedTo [:>]`
    TranscribedTo(Vec<Argument>),

    /// RNA is translated to protein
    /// * `translatedTo [>>]`
    TranslatedTo(Vec<Argument>),
}

impl Relation {
    /// * `analogous`
    pub fn analogous(left: Argument, right: Argument) -> Self {
        Relation::Analogous(vec![left, right])
    }

    /// * `association [--]`
    pub fn association(left: Argument, right: Argument) -> Self {
        Relation::Association(vec![left, right])
    }

    /// * `biomarkerFor`
    pub fn biomarker_for(left: Argument, right: Argument) -> Self {
        Relation::BiomarkerFor(vec![left, right])
    }

    /// * `causesNoChange [cnc]`
    pub fn causes_no_change(left: Argument, right: Argument) -> Self {
        Relation::CausesNoChange(vec![left, right])
    }

    /// * `decreases [-|]`
    pub fn decreases(left: Argument, right: Argument) -> Self {
        Relation::Decreases(vec![left, right])
    }

    /// * `directlyDecreases [=|]`
    pub fn directly_decreases(left: Argument, right: Argument) -> Self {
        Relation::DirectlyDecreases(vec![left, right])
    }

    /// * `directlyIncreases [=>]`
    pub fn directly_increases(left: Argument, right: Argument) -> Self {
        Relation::DirectlyIncreases(vec![left, right])
    }

    /// * `hasActivity`
    pub fn has_activity(left: Argument, right: Argument) -> Self {
        Relation::HasActivity(vec![left, right])
    }

    /// * `hasComponent`
    pub fn has_component(left: Argument, right: Argument) -> Self {
        Relation::HasComponent(vec![left, right])
    }

    /// * `hasComponents`
    pub fn has_components(left: Argument, right: Argument) -> Self {
        Relation::HasComponents(vec![left, right])
    }

    /// * `hasMember`
    pub fn has_member(left: Argument, right: Argument) -> Self {
        Relation::HasMember(vec![left, right])
    }

    /// * `hasMembers`
    pub fn has_members(left: Argument, right: Argument) -> Self {
        Relation::HasMembers(vec![left, right])
    }

    /// * `increases [->]`
    pub fn increases(left: Argument, right: Argument) -> Self {
        Relation::Increases(vec![left, right])
    }

    /// * `isA`
    pub fn is_a(left: Argument, right: Argument) -> Self {
        Relation::IsA(vec![left, right])
    }

    /// * `negativeCorrelation [neg]`
    pub fn negative_correlation(left: Argument, right: Argument) -> Self {
        Relation::NegativeCorrelation(vec![left, right])
    }

    /// * `orthologous`
    pub fn orthologous(left: Argument, right: Argument) -> Self {
        Relation::Orthologous(vec![left, right])
    }

    /// * `positiveCorrelation [pos]`
    pub fn positive_correlation(left: Argument, right: Argument) -> Self {
        Relation::PositiveCorrelation(vec![left, right])
    }

    /// * `prognosticBiomarkerFor`
    pub fn prognostic_biomarker_for(left: Argument, right: Argument) -> Self {
        Relation::PrognosticBiomarkerFor(vec![left, right])
    }

    /// * `rateLimitingStepOf`
    pub fn rate_limiting_step_of(left: Argument, right: Argument) -> Self {
        Relation::RateLimitingStepOf(vec![left, right])
    }

    /// * `regulates [reg]`
    pub fn regulates(left: Argument, right: Argument) -> Self {
        Relation::Regulates(vec![left, right])
    }

    /// * `subProcessOf`
    pub fn sub_process_of(left: Argument, right: Argument) -> Self {
        Relation::SubProcessOf(vec![left, right])
    }

    /// * `transcribedTo [:>]`
    pub fn transcribed_to(left: Argument, right: Argument) -> Self {
        Relation::TranscribedTo(vec![left, right])
    }

    /// * `translatedTo [>>]`
    pub fn translated_to(left: Argument, right: Argument) -> Self {
        Relation::TranslatedTo(vec![left, right])
    }

}


pub fn main() {
    let term = Relation::directly_decreases(
        Argument::fun(Function::p1(Argument::ns("PFH", "Hedgehog Family"))),
        Argument::fun(Function::p1(Argument::ns("HGNC", "PTCH1"))),
    );
    
    /// repression of the transcription of miR-21 by FOXO3 protein transcriptional activity
    let term = Relation::directly_decreases(
        Argument::fun(Function::act2(
            Argument::fun(Function::p1(Argument::ns(
                "HGNC", "FOXO3"
            ))),
            Argument::fun(Function::ma1(Argument::ns(
                "DEFAULT", "tscript"
            ))),
        )),
        Argument::fun(Function::r1(Argument::ns("HGNC", "MIR21"))),
    );

    
    println!("{:?}", term);
}