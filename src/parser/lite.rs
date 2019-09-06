// The point of this parser is to parse a program with enough fidelity to identify the head
// positions and pipeline elements, but to be agnostic to the difference between internal
// and external commands.
//
// External commands treat every token as a bare word unless it starts with `$` or is `|`
// Internal commands have a rich vocabulary of literals and operators.

mod builder;
mod delimited_node;
mod list_node;
mod path_node;
mod pipeline_node;
mod tokens;

use self::list_node::ListNode;
use self::pipeline_node::PipelineElement;
use self::tokens::LiteNode;
use crate::prelude::*;
use builder::LiteBuilder;
use derive_new::new;
use log::trace;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::InputIter;
use nom::{Compare, IResult, InputLength, InputTake, UnspecializedInput};
use nom5_locate::LocatedSpan;
use std::fmt::Debug;
pub use std::str::{CharIndices, Chars};
use uuid::Uuid;

#[derive(PartialEq, Debug, Clone, Copy, new)]
pub struct Input<'a> {
    nom: LocatedSpan<&'a str>,
    origin: Uuid,
}

impl<'a> Input<'a> {
    #[cfg(test)]
    pub fn string(s: &'a str, origin: Uuid) -> Input<'a> {
        Input {
            nom: LocatedSpan::new(s.as_ref()),
            origin,
        }
    }

    fn offset(&self) -> usize {
        self.nom.offset
    }
}

impl<'a> From<Input<'a>> for Tag {
    fn from(input: Input<'a>) -> Tag {
        input.tag()
    }
}

impl<'a> InputIter for Input<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.nom.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.nom.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.nom.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        self.nom.slice_index(count)
    }
}

impl<'a> InputLength for Input<'a> {
    fn input_len(&self) -> usize {
        self.nom.input_len()
    }
}

impl<'a> InputTake for Input<'a> {
    fn take(&self, count: usize) -> Self {
        let result = self.nom.take(count);

        Input::new(result, self.origin)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.nom.take_split(count);

        (
            Input::new(left, self.origin),
            Input::new(right, self.origin),
        )
    }
}

impl<'a> Input<'a> {
    fn tag_starting_with(&self, start: usize) -> Tag {
        Tag::from((start, self.nom.offset, self.origin))
    }

    fn tag(&self) -> Tag {
        Tag::from((
            self.nom.offset,
            self.nom.offset + self.nom.fragment.len(),
            self.origin,
        ))
    }
}

impl<'a> UnspecializedInput for Input<'a> {}

impl<'a> Compare<&'a str> for Input<'a> {
    fn compare(&self, other: &str) -> nom::CompareResult {
        self.nom.compare(other)
    }

    fn compare_no_case(&self, other: &str) -> nom::CompareResult {
        self.nom.compare_no_case(other)
    }
}

fn word(input: Input) -> IResult<Input, LiteNode> {
    trace_step(input, "word", move |input| {
        let start = input.offset();
        let (input, _) = take_while(is_word_char)(input)?;

        Ok((
            input,
            LiteBuilder::spanned_word(input.tag_starting_with(start)),
        ))
    })
}

pub fn node(input: Input) -> IResult<Input, LiteNode> {
    trace_step(input, "node", word)
}

pub fn raw_call(input: Input) -> IResult<Input, Vec<LiteNode>> {
    trace_step(input, "raw_call", move |input| {
        let left = input.offset();
        let (input, items) = token_list(input)?;
        let right = input.offset();

        Ok((input, items))
    })
}

pub fn token_list(input: Input) -> IResult<Input, Vec<LiteNode>> {
    trace_step(input, "token_list", move |input| {
        let (input, first) = node(input)?;
        let (input, list) = many0(pair(space1, node))(input)?;

        Ok((input, make_token_list(None, first, list, None)))
    })
}

fn make_token_list(
    sp_left: Option<Input>,
    first: LiteNode,
    list: Vec<(Input, LiteNode)>,
    sp_right: Option<Input>,
) -> Vec<LiteNode> {
    let mut nodes = vec![];

    if let Some(sp_left) = sp_left {
        nodes.push(LiteNode::Whitespace(sp_left.tag()));
    }

    nodes.push(first);

    for (ws, token) in list {
        nodes.push(LiteNode::Whitespace(ws.tag()));
        nodes.push(token);
    }

    if let Some(sp_right) = sp_right {
        nodes.push(LiteNode::Whitespace(sp_right.tag()));
    }

    nodes
}

pub fn pipeline(input: Input) -> IResult<Input, LiteNode> {
    trace_step(input, "pipeline", |input| {
        let start = input.offset();
        let (input, head) = opt(tuple((raw_call, opt(space1), opt(tag("|")))))(input)?;
        // let (input, items) = trace_step(
        //     input,
        //     "many0",
        //     many0(tuple((opt(space1), raw_call, opt(space1), opt(tag("|"))))),
        // )?;

        let (input, tail) = opt(space1)(input)?;
        let (input, _) = opt(multispace1)(input)?;

        if input.input_len() != 0 {
            return Err(nom::Err::Error(nom::error_position!(
                input,
                nom::error::ErrorKind::Eof
            )));
        }

        let end = input.offset();

        Ok((
            input,
            LiteBuilder::spanned_pipeline(
                (make_call_list(head, vec![]), tail.map(Tag::from)),
                (start, end, input.origin),
            ),
        ))
    })
}

fn make_call_list(
    head: Option<(Vec<LiteNode>, Option<Input>, Option<Input>)>,
    items: Vec<(Option<Input>, Vec<LiteNode>, Option<Input>, Option<Input>)>,
) -> Vec<PipelineElement> {
    let mut out = vec![];

    if let Some(head) = head {
        let el = PipelineElement::new(None, head.0, head.1.map(Tag::from), head.2.map(Tag::from));
        out.push(el);
    }

    for (ws1, call, ws2, pipe) in items {
        let el = PipelineElement::new(
            ws1.map(Tag::from),
            call,
            ws2.map(Tag::from),
            pipe.map(Tag::from),
        );
        out.push(el);
    }

    out
}

fn is_word_char(c: char) -> bool {
    match c {
        '|' | '$' | '"' | '\'' | '#' => false,
        other if other.is_whitespace() => false,
        _ => true,
    }
}

fn trace_step<'a, T: Debug>(
    input: Input<'a>,
    name: &str,
    block: impl FnOnce(Input<'a>) -> IResult<Input<'a>, T>,
) -> IResult<Input<'a>, T> {
    trace!(target: "nu::lite_parse", "+ before {} @ {:?}", name, input);
    match block(input) {
        Ok((input, result)) => {
            trace!(target: "nu::lite_parse", "after {} @ {:?} -> {:?}", name, input, result);
            Ok((input, result))
        }

        Err(e) => {
            trace!(target: "nu::lite_parse", "- failed {} :: {:?}", name, e);
            Err(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lite::builder::{CurriedNode, LiteBuilder as b, ToLiteNode};
    use pretty_assertions::assert_eq;

    fn build_token(block: impl ToLiteNode) -> LiteNode {
        let mut builder = LiteBuilder::new(test_uuid());
        block.to_token(&mut builder)
    }

    macro_rules! test {
        ($name:tt $expr:expr) => {
            #[test]
            fn $name() {
                let _ = pretty_env_logger::try_init();
                $expr
            }
        };
    }

    test! {
        test_lite_word
        {
            let words = "hello +nightly 1.6 0.2.2 x&y + & world!".split(" ");

            for word in words {
                assert_eq!(apply(node, "node", word), build_token(b::word(word)));
                assert_eq!(apply(pipeline, "pipeline", word), build_token(b::pipeline(vec![(None, vec![b::word(word)], None)])));
            }
        }
    }

    test! {
        test_delimited_paren
        {
            assert_eq!(
                apply(node, "node", "(abc)"),
                build_token(b::parens(vec![&b::word("abc")]))
            );

            assert_eq!(
                apply(node, "node", "(  abc  )"),
                build_token(b::parens(vec![b::ws("  "), b::word("abc"), b::ws("  ")]))
            );

            assert_eq!(
                apply(node, "node", "(  abc def )"),
                build_token(b::parens(vec![
                    b::ws("  "),
                    b::word("abc"),
                    b::sp(),
                    b::word("def"),
                    b::sp()
                ]))
            );

            assert_eq!(
                apply(node, "node", "(  abc def 123 456GB )"),
                build_token(b::parens(vec![
                    b::ws("  "),
                    b::word("abc"),
                    b::sp(),
                    b::word("def"),
                    b::sp(),
                    b::word("123"),
                    b::sp(),
                    b::unit("456", "GB"),
                    b::sp()
                ]))
            );
        }
    }

    #[test]
    fn test_delimited_square() {
        assert_eq!(
            apply(node, "node", "[abc]"),
            build_token(b::square(vec![b::word("abc")]))
        );

        assert_eq!(
            apply(node, "node", "[  abc  ]"),
            build_token(b::square(vec![b::ws("  "), b::word("abc"), b::ws("  ")]))
        );

        assert_eq!(
            apply(node, "node", "[  abc def ]"),
            build_token(b::square(vec![
                b::ws("  "),
                b::word("abc"),
                b::sp(),
                b::word("def"),
                b::sp()
            ]))
        );

        assert_eq!(
            apply(node, "node", "[  abc def 123 456GB ]"),
            build_token(b::square(vec![
                b::ws("  "),
                b::word("abc"),
                b::sp(),
                b::word("def"),
                b::sp(),
                b::word("123"),
                b::sp(),
                b::unit("456", "GB"),
                b::sp()
            ]))
        );
    }

    fn apply<T>(
        f: impl Fn(Input) -> Result<(Input, T), nom::Err<(Input, nom::error::ErrorKind)>>,
        desc: &str,
        string: &str,
    ) -> T {
        match f(Input::string(string, test_uuid())) {
            Ok(v) => v.1,
            Err(other) => {
                println!("{:?}", other);
                println!("for {} @ {}", string, desc);
                panic!("No dice");
            }
        }
    }

    fn test_uuid() -> Uuid {
        let uuid = uuid::Builder::nil()
            .set_variant(uuid::Variant::RFC4122)
            .set_version(uuid::Version::Random)
            .build();

        uuid
    }
}
