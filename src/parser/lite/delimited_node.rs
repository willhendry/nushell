use crate::parser::lite::tokens::LiteNode;
use derive_new::new;
use enum_utils::FromStr;
use getset::Getters;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters, new)]
#[get = "pub(crate)"]
pub struct DelimitedNode {
    delimiter: Delimiter,
    children: Vec<LiteNode>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, FromStr)]
pub enum Delimiter {
    Paren,
    Brace,
    Square,
}
