use crate::parser::lite::list_node::ListNode;
use crate::parser::lite::tokens::LiteNode;
use crate::{Tag, Tagged};
use derive_new::new;
use getset::Getters;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, new)]
pub struct Pipeline {
    pub(crate) parts: Vec<PipelineElement>,
    pub(crate) post_ws: Option<Tag>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters, new)]
pub struct PipelineElement {
    pub pre_ws: Option<Tag>,
    #[get = "pub(crate)"]
    words: Vec<LiteNode>,
    pub post_ws: Option<Tag>,
    pub post_pipe: Option<Tag>,
}
