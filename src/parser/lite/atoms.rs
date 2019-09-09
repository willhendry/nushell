use crate::parser::lite::graphemes::{Grapheme, GraphemeSpan};
use crate::parser::lite::{
    builder::LiteBuilder, list_node::ListNode, pipeline_node::PipelineElement, tokens::LiteNode,
};
use crate::prelude::*;
use derive_new::new;
use log::trace;
use nom::bytes::complete::{tag, take_while1};
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::InputIter;
use nom::{Compare, IResult, InputLength, InputTake, UnspecializedInput};
use nom5_locate::impl_input_iter;
use nom5_locate::LocatedSpan;
use std::fmt::Debug;
pub use std::str::{CharIndices, Chars};
use unicode_segmentation::{GraphemeIndices, Graphemes};
use uuid::Uuid;

type Input<'a> = GraphemeSpan<'a, Uuid>;

impl<'a> Input<'a> {
    pub(crate) fn tag_from(&self, start: usize) -> Tag {
        Tag::from((start, *self.offset(), *self.extra()))
    }
}

#[derive(Eq, PartialEq, Debug)]
enum TokenContents {
    Atom(Atom),
}

type Token = Tagged<TokenContents>;

#[derive(Eq, PartialEq, Debug)]
pub enum Atom {
    // Whitespace is distinct from Newline (which is treated as a separator). Dialects
    // are free to be whitespace sensitive (for example, `+nightly` is distinct from
    // `+ nightly` in the external dialect)
    Whitespace,

    // Newline and semicolon are treated equivalently as "separators" at the reader level
    Newline,
    Semicolon,

    // Comments are semantically ignored, but may be attached to HIR nodes by dialects
    Comment,

    // Words are almost always further processed by dialects
    Word,

    // Flags are distinct from words: they can serve as boundary markers in the expansion
    // algorithms.
    ShortFlag(Tag),
    LongFlag(Tag),

    // Strings are special because they require special support in the reader in order
    // to properly identify the boundaries of the string.
    DoubleQuotedString(Tag),
    SingleQuotedString(Tag),

    // Variables are special because dereferencing is agnostic to dialect
    Variable(Tag),
}

fn semicolon(input: Input) -> IResult<Input, Tagged<Atom>> {
    let start = input.offset();
    let (input, string) = tag(";")(input)?;

    Ok((
        input,
        Atom::Semicolon.tagged((*start, *input.offset(), *input.extra())),
    ))
}

fn word(input: Input) -> IResult<Input, Tagged<Atom>> {
    println!("Trying word for {:?}", input);
    let start = input.offset();
    let (input, string) = take_while1(is_word_char)(input)?;

    println!("Succeeded, new-input :: {:?}", input);

    Ok((
        input,
        Atom::Word.tagged((*start, *input.offset(), *input.extra())),
    ))
}

fn whitespace(input: Input) -> IResult<Input, Tagged<Atom>> {
    println!("Trying whitespace for {:?}", input);

    let start = input.offset();
    let (input, string) = take_while1(is_whitespace_char)(input)?;

    println!("Succeeded, new-input :: {:?}", input);

    Ok((input, Atom::Whitespace.tagged(input.tag_from(start))))
}

fn is_word_char(c: Grapheme) -> bool {
    match c.grapheme {
        "|" | "$" | "\"" | "'" | "#" => false,
        other => !other.chars().all(|c| c.is_whitespace()),
    }
}

fn is_whitespace_char(c: Grapheme) -> bool {
    c.grapheme.chars().all(|c| c.is_whitespace())
}
