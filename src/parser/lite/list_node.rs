use crate::parser::lite::tokens::LiteNode;
use getset::Getters;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters)]
pub struct ListNode {
    #[get = "pub(crate)"]
    items: Option<Vec<LiteNode>>,
}
