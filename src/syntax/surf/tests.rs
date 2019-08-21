use super::{parse_expr_err_printed, parse_str_err_printed};

macro_rules! success {
    ($str:literal) => {
        parse_str_err_printed($str)
            .map(|ast| println!("{:?}", ast))
            .unwrap();
    };
}

macro_rules! success_expr {
    ($str:literal) => {
        parse_expr_err_printed($str)
            .map(|ast| println!("{:?}", ast))
            .unwrap();
    };
}

#[test]
fn simple_declaration_parsing() {
    success!("val a : b;");
    parse_str_err_printed("val a : b").unwrap_err();
    parse_str_err_printed("a : b").unwrap_err();
    success!("let a = b;");
    parse_str_err_printed("a = b").unwrap_err();
}

#[test]
fn primary_expr_parsing() {
    success!("let a = Type;");
    success!("let a = Type233;");
    success!("let van = (Type233);");
    parse_str_err_printed("let darkholm = (Type233;").unwrap_err();
}

#[test]
fn pi_type_parsing() {
    success!("val mayori : monika -> (a: A) -> (c d e : CDE) -> F;");
    success!("val star : platinum * (a: A) * F;");
}

#[test]
fn standalone_expr_parsing() {
    success_expr!("Agda");
    success_expr!("Rec { n : Idris; }");
    success_expr!("Sum { n : Idris; }");
    success_expr!("Rec { n : Coq; ... = Epigram }");
    success_expr!("Sum { n : Coq; ... = Epigram }");
    success_expr!("Sum { n : HOL; m : Isabelle; ... = Epigram }");
}

#[test]
fn simple_expr_parsing() {
    success!("let reimu = marisa|>alice;");
    success!("let madoka = homura sayaka kyoko;");
    success!("let touma = \\kazusa Setsuna. Ogiso;");
    success!("let komeji = satori$koishi orin$okku;");
    success!("let scarlet = devil, mansion;");
    success!("val deep : @Dark Fantasy;");
    success!("val uw : ^^^pl se;");
}

#[test]
fn row_polymorphic_type_def_expr_parsing() {
    success!("val king : Rec {n: A;};");
    success!("val crimson : Sum {};");
    success!("val experience : Rec {};");
}
