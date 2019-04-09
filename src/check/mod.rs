use crate::syntax::surf::ast::Decl;

pub type TCS<'a> = ();

// TODO:
//  type-checking state,
//  type-checking error,
//  type-checking monad
pub fn check_main<'a>(_decls: Vec<Decl>) -> Result<TCS<'a>, String> {
    unimplemented!()
}
