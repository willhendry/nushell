use crate::parser::lite::tokens::LiteNode;
use derive_new::new;
use getset::Getters;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters, new)]
#[get = "pub(crate)"]
pub struct PathNode {
    head: Box<LiteNode>,
    tail: Vec<LiteNode>,
}
