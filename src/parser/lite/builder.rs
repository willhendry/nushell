use crate::parser::lite::{
    delimited_node::{DelimitedNode, Delimiter},
    list_node::ListNode,
    pipeline_node::{Pipeline, PipelineElement},
    tokens::{LiteNode, RawLiteToken},
};
use crate::parser::Unit;
use crate::{Tag, Tagged, TaggedItem};
use derive_new::new;
use uuid::Uuid;

#[derive(Clone)]
pub enum CurriedNode {
    Whitespace(CurriedWhitespace),
    Word(CurriedWord),
}

impl ToLiteNode for CurriedNode {
    fn to_token(&self, builder: &mut LiteBuilder) -> LiteNode {
        match self {
            CurriedNode::Whitespace(ws) => ws.to_token(builder),
            CurriedNode::Word(word) => word.to_token(builder),
        }
    }

    fn to_curried_node(&self) -> CurriedNode {
        self.clone()
    }
}

#[derive(new)]
pub struct LiteBuilder {
    #[new(default)]
    pos: usize,

    origin: Uuid,
}

pub trait ToLiteNode {
    fn to_token(&self, builder: &mut LiteBuilder) -> LiteNode;
    fn to_curried_node(&self) -> CurriedNode;
}

#[derive(new, Clone)]
pub struct CurriedWhitespace {
    value: String,
}

impl ToLiteNode for CurriedWhitespace {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let (start, end, origin) = b.consume(&self.value);
        LiteBuilder::spanned_ws((start, end, origin))
    }

    fn to_curried_node(&self) -> CurriedNode {
        CurriedNode::Whitespace(self.clone())
    }
}

#[derive(new, Clone)]
pub struct CurriedWord {
    input: String,
}

impl ToLiteNode for CurriedWord {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let (start, end, _) = b.consume(&self.input);
        b.pos = end;

        LiteBuilder::spanned_word((start, end, b.origin))
    }

    fn to_curried_node(&self) -> CurriedNode {
        CurriedNode::Word(self.clone())
    }
}

impl LiteBuilder {
    #[allow(unused)]
    pub fn build(origin: Uuid, block: impl FnOnce(&mut Self) -> LiteNode) -> LiteNode {
        let mut builder = LiteBuilder::new(origin);
        block(&mut builder)
    }

    #[allow(unused)]
    pub fn word(input: impl Into<String>) -> CurriedWord {
        CurriedWord::new(input.into())
    }

    pub fn spanned_word(input: impl Into<Tag>) -> LiteNode {
        LiteNode::Token(RawLiteToken::Word.tagged(input))
    }

    #[allow(unused)]
    pub fn sp() -> CurriedWhitespace {
        CurriedWhitespace::new(" ".into())
    }

    #[allow(unused)]
    pub fn ws(input: impl Into<String>) -> CurriedWhitespace {
        CurriedWhitespace::new(input.into())
    }

    pub fn spanned_ws(tag: impl Into<Tag>) -> LiteNode {
        let tag = tag.into();

        LiteNode::Whitespace(tag.into())
    }

    fn consume(&mut self, input: &str) -> (usize, usize, Uuid) {
        let start = self.pos;
        self.pos += input.len();
        (start, self.pos, self.origin)
    }
}
