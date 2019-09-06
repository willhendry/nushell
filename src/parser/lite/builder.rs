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
    Word(CurriedWord),
    Unit(CurriedUnit),
    Delimited(CurriedDelimited),
    Nodes(CurriedNodes),
    PipelineElement(CurriedNodes),
}

impl ToLiteNode for CurriedNode {
    fn to_token(&self, builder: &mut LiteBuilder) -> LiteNode {
        match self {
            CurriedNode::Word(word) => word.to_token(builder),
            CurriedNode::Unit(unit) => unit.to_token(builder),
            CurriedNode::Delimited(delimited) => delimited.to_token(builder),
            CurriedNode::Nodes(nodes) => nodes.to_token(builder),
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

#[derive(new, Clone)]
pub struct CurriedUnit {
    value: String,
    unit: Unit,
}

impl ToLiteNode for CurriedUnit {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let (start_value, end_value, _) = b.consume(&self.value.to_string());
        let (_, end_unit, _) = b.consume(self.unit.as_str());
        b.pos = end_unit;

        LiteBuilder::spanned_unit(
            ((start_value, end_value, b.origin), self.unit),
            (start_value, end_unit, b.origin),
        )
    }

    fn to_curried_node(&self) -> CurriedNode {
        CurriedNode::Unit(self.clone())
    }
}

#[derive(new, Clone)]
pub struct CurriedDelimited {
    tokens: Vec<CurriedNode>,
    delimiter: Delimiter,
}

impl ToLiteNode for CurriedDelimited {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let (start, _, _) = b.consume("(");
        let mut output = vec![];
        for token in self.tokens {
            output.push(token.to_token(b));
        }

        let (_, end, origin) = b.consume(")");

        LiteBuilder::spanned_delimited(output, self.delimiter, (start, end, origin))
    }

    fn to_curried_node(&self) -> CurriedNode {
        CurriedNode::Delimited(self.clone())
    }
}

#[derive(new, Clone)]
pub struct CurriedNodes {
    tokens: Vec<CurriedNode>,
}

impl ToLiteNode for CurriedNodes {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let start = b.pos;

        let mut nodes = vec![];
        for token in self.tokens {
            nodes.push(token.to_token(b));
        }

        let end = b.pos;

        LiteBuilder::spanned_nodes(nodes, (start, end, b.origin))
    }

    fn to_curried_node(&self) -> CurriedNode {
        CurriedNode::Nodes(self.clone())
    }
}

#[derive(new)]
struct CurriedPipeline {
    elements: Vec<CurriedPipelineElement>,
}

impl ToLiteNode for CurriedPipeline {
    fn to_token(&self, b: &mut LiteBuilder) -> LiteNode {
        let start = b.pos;

        let mut out: Vec<PipelineElement> = vec![];

        let mut input = self.elements.into_iter().peekable();
        let element = input
            .next()
            .expect("A pipeline must contain at least one element");

        let pre_span = element.pre_ws.map(|pre| b.consume(&pre));
        let tokens = element.nodes.iter().map(|token| token(b)).collect();
        let post_span = element.post_ws.map(|post| b.consume(&post));
        let pipe = input.peek().map(|_| Tag::from(b.consume("|")));

        out.push(PipelineElement::new(
            pre_span.map(Tag::from),
            tokens,
            post_span.map(Tag::from),
            pipe,
        ));

        loop {
            match input.next() {
                None => break,
                Some((pre, tokens, post)) => {
                    let pre_span = pre.map(|pre| b.consume(&pre));
                    let tokens = tokens.iter().map(b).collect();
                    let post_span = post.map(|post| b.consume(&post));

                    let pipe = input.peek().map(|_| Tag::from(b.consume("|")));

                    out.push(PipelineElement::new(
                        pre_span.map(Tag::from),
                        tokens,
                        post_span.map(Tag::from),
                        pipe,
                    ));
                }
            }
        }

        let end = b.pos;

        LiteBuilder::spanned_pipeline((out, None), (start, end, b.origin))
    }
    fn to_curried_node(&self) -> CurriedNode {
        unimplemented!()
    }
}

struct CurriedPipelineElement {
    pre_ws: Option<String>,
    nodes: Vec<CurriedNode>,
    post_ws: Option<String>,
}

impl CurriedPipelineElement {
    fn to_element(&self, b: &mut LiteBuilder) -> PipelineElement {
        let pre_tag = self.pre_ws.map(|pre| b.consume(&pre));
        let tokens = self.nodes.iter().map(|node| node.to_token(b)).collect();
        let post_tag = self.post_ws.map(|post| b.consume(&post));

        PipelineElement::new(pre_tag, tokens, post_tag, None)
    }
}

impl LiteBuilder {
    #[allow(unused)]
    pub fn build(origin: Uuid, block: impl FnOnce(&mut Self) -> LiteNode) -> LiteNode {
        let mut builder = LiteBuilder::new(origin);
        block(&mut builder)
    }

    #[allow(unused)]
    pub fn pipeline(
        input: Vec<(Option<Tag>, Vec<&dyn ToLiteNode>, Option<Tag>)>,
    ) -> CurriedPipeline {
        let input: Vec<(Option<String>, Vec<CurriedToken>, Option<String>)> = input
            .into_iter()
            .map(|el| {
                let (pre, token, post) = el.into();

                (
                    pre.map(|s| s.to_string()),
                    token,
                    post.map(|s| s.to_string()),
                )
            })
            .collect();

        Box::new(move |b| {
            let start = b.pos;

            let mut out: Vec<PipelineElement> = vec![];

            let mut input = input.into_iter().peekable();
            let (pre, tokens, post) = input
                .next()
                .expect("A pipeline must contain at least one element");

            let pre_span = pre.map(|pre| b.consume(&pre));
            let tokens = tokens.iter().map(|token| token(b)).collect();
            let post_span = post.map(|post| b.consume(&post));
            let pipe = input.peek().map(|_| Tag::from(b.consume("|")));
            out.push(PipelineElement::new(
                pre_span.map(Tag::from),
                tokens,
                post_span.map(Tag::from),
                pipe,
            ));

            loop {
                match input.next() {
                    None => break,
                    Some((pre, tokens, post)) => {
                        let pre_span = pre.map(|pre| b.consume(&pre));
                        let tokens = tokens.iter().map(b).collect();
                        let post_span = post.map(|post| b.consume(&post));

                        let pipe = input.peek().map(|_| Tag::from(b.consume("|")));

                        out.push(PipelineElement::new(
                            pre_span.map(Tag::from),
                            tokens,
                            post_span.map(Tag::from),
                            pipe,
                        ));
                    }
                }
            }

            let end = b.pos;

            LiteBuilder::spanned_pipeline((out, None), (start, end, b.origin))
        })
    }

    pub fn spanned_pipeline(
        input: (Vec<PipelineElement>, Option<Tag>),
        tag: impl Into<Tag>,
    ) -> LiteNode {
        LiteNode::Pipeline(Pipeline::new(input.0, input.1.into()).tagged(tag))
    }

    #[allow(unused)]
    pub fn word(input: impl Into<String>) -> CurriedWord {
        CurriedWord::new(input.into())
    }

    pub fn spanned_word(input: impl Into<Tag>) -> LiteNode {
        LiteNode::Token(RawLiteToken::Word.tagged(input))
    }

    #[allow(unused)]
    pub fn unit(value: impl Into<String>, unit: impl Into<Unit>) -> CurriedUnit {
        let value = value.into();
        let unit = unit.into();

        CurriedUnit::new(value, unit)
    }

    pub fn spanned_unit(input: (impl Into<Tag>, impl Into<Unit>), tag: impl Into<Tag>) -> LiteNode {
        let (value, unit) = (input.0.into(), input.1.into());

        LiteNode::Token(RawLiteToken::Size(value, unit).tagged(tag))
    }

    #[allow(unused)]
    pub fn nodes(input: Vec<&dyn ToLiteNode>) -> CurriedNodes {
        CurriedNodes::new(input.iter().map(|t| t.to_curried_node()).collect())
    }

    pub fn spanned_nodes(input: Vec<LiteNode>, tag: impl Into<Tag>) -> LiteNode {
        if input.len() == 0 {
            panic!("BUG: spanned call (TODO)")
        }

        LiteNode::List(input.tagged(tag))
    }

    #[allow(unused)]
    pub fn parens(input: Vec<&dyn ToLiteNode>) -> CurriedDelimited {
        CurriedDelimited::new(input, Delimiter::Paren)
    }

    pub fn spanned_delimited(
        input: impl Into<Vec<LiteNode>>,
        delimiter: Delimiter,
        tag: impl Into<Tag>,
    ) -> LiteNode {
        LiteNode::Delimited(DelimitedNode::new(delimiter, input.into()).tagged(tag))
    }

    #[allow(unused)]
    pub fn square(input: Vec<&dyn ToLiteNode>) -> CurriedToken {
        Box::new(move |b| {
            let (start, _, _) = b.consume("[");
            let mut output = vec![];
            for item in input {
                output.push(item(b));
            }

            let (_, end, origin) = b.consume("]");

            LiteBuilder::spanned_square(output, (start, end, origin))
        })
    }

    pub fn spanned_square(input: impl Into<Vec<LiteNode>>, tag: impl Into<Tag>) -> LiteNode {
        LiteNode::Delimited(DelimitedNode::new(Delimiter::Square, input.into()).tagged(tag))
    }

    #[allow(unused)]
    pub fn braced(input: Vec<CurriedToken>) -> CurriedToken {
        Box::new(move |b| {
            let (start, _, _) = b.consume("{ ");
            let mut output = vec![];
            for item in input {
                output.push(item(b));
            }

            let (_, end, origin) = b.consume(" }");

            LiteBuilder::spanned_brace(output, (start, end, origin))
        })
    }

    pub fn spanned_brace(input: impl Into<Vec<LiteNode>>, tag: impl Into<Tag>) -> LiteNode {
        LiteNode::Delimited(DelimitedNode::new(Delimiter::Brace, input.into()).tagged(tag))
    }

    #[allow(unused)]
    pub fn sp() -> CurriedToken {
        Box::new(|b| {
            let (start, end, origin) = b.consume(" ");
            LiteNode::Whitespace(Tag::from((start, end, origin)))
        })
    }

    #[allow(unused)]
    pub fn ws(input: impl Into<String>) -> CurriedToken {
        let input = input.into();

        Box::new(move |b| {
            let (start, end, origin) = b.consume(&input);
            LiteBuilder::spanned_ws((start, end, origin))
        })
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
