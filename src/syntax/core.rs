type Level = u32;

#[derive(Debug, PartialEq, Eq)]
pub struct Term {
    pub info: TermInfo,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TermInfo {
    Type(Level),
}
