use super::{parse_expr_err_printed, parse_str_err_printed};

#[test]
fn simple_declaration_parsing() {
    parse_str_err_printed("val a : b;").unwrap();
    parse_str_err_printed("val a : b").unwrap_err();
    parse_str_err_printed("a : b").unwrap_err();
    parse_str_err_printed("let a = b;").unwrap();
    parse_str_err_printed("a = b").unwrap_err();
    parse_str_err_printed("a = !").unwrap_err();
}

#[test]
fn primary_expr_parsing() {
    parse_str_err_printed("let a = Type;").unwrap();
    parse_str_err_printed("let a = Type233;").unwrap();
    parse_str_err_printed("let van = (Type233);").unwrap();
    parse_str_err_printed("let darkholm = (Type233;").unwrap_err();
}

#[test]
fn pi_type_parsing() {
    parse_str_err_printed("val mayori : monika -> (a: A) -> (c d e : CDE) -> F;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("val star : platinum * (a: A) * F;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
}

#[test]
fn standalone_expr_parsing() {
    parse_expr_err_printed("Agda").unwrap();
    parse_expr_err_printed("Rec { n : Idris; }")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_expr_err_printed("Sum { n : Idris; }")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_expr_err_printed("Rec { n : Coq; ... = Epigram }")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_expr_err_printed("Sum { n : Coq; ... = Epigram }")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_expr_err_printed("Sum { n : HOL; m : Isabelle; ... = Epigram }")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
}

#[test]
fn simple_expr_parsing() {
    parse_str_err_printed("let reimu = marisa|>alice;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("let madoka = homura sayaka kyoko;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("let touma = \\kazusa Setsuna. Ogiso;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("let komeji = satori$koishi orin$okku;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("let scarlet = devil, mansion;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("val deep : @Dark Fantasy;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
    parse_str_err_printed("val uw : ^^^pl se;")
        .map(|ast| println!("{:?}", ast))
        .unwrap();
}
