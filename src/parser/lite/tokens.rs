use crate::errors::ShellError;
use crate::object::meta::{Tag, Tagged};
use crate::parser::lite::delimited_node::DelimitedNode;
use crate::parser::lite::list_node::ListNode;
use crate::parser::lite::path_node::PathNode;
use crate::parser::lite::pipeline_node::Pipeline;
use crate::parser::Unit;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum RawLiteToken {
    Word,
    Size(Tag, Unit),
}

pub type Token = Tagged<RawLiteToken>;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum LiteNode {
    Token(Token),

    List(Tagged<ListNode>),
    Delimited(Tagged<DelimitedNode>),
    Pipeline(Tagged<Pipeline>),
    Path(Tagged<PathNode>),

    Whitespace(Tag),
    Error(Tagged<Box<ShellError>>),
}
