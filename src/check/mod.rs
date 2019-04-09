use crate::syntax::surf::ast::Declaration;

pub type TCS<'a> = ();

// TODO:
//  type-checking state,
//  type-checking error,
//  type-checking monad
pub fn check_main<'a>(_decls: Vec<Declaration>) -> Result<TCS<'a>, String> {
    unimplemented!()
}
