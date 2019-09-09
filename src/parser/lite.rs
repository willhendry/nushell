// The point of this parser is to parse a program with enough fidelity to identify the head
// positions and pipeline elements, but to be agnostic to the difference between internal
// and external commands.
//
// External commands treat every token as a bare word unless it starts with `$` or is `|`
// Internal commands have a rich vocabulary of literals and operators.

mod atoms;
mod builder;
mod delimited_node;
mod graphemes;
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
use nom5_locate::impl_input_iter;
use nom5_locate::LocatedSpan;
use std::fmt::Debug;
pub use std::str::{CharIndices, Chars};
use unicode_segmentation::{GraphemeIndices, Graphemes};
use uuid::Uuid;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lite::builder::{CurriedNode, LiteBuilder as b, ToLiteNode};
    use crate::parser::lite::graphemes::GraphemeSpan;
    use pretty_assertions::assert_eq;
    use uuid::Uuid;

    type Input<'a> = GraphemeSpan<'a, Uuid>;

    fn build_token(block: impl ToLiteNode) -> LiteNode {
        let mut builder = LiteBuilder::new(test_uuid());
        block.to_token(&mut builder)
    }

    fn apply<'a, T>(
        f: impl Fn(Input<'a>) -> Result<(Input<'a>, T), nom::Err<(Input<'a>, nom::error::ErrorKind)>>,
        desc: &str,
        string: &'a str,
    ) -> T {
        match f(GraphemeSpan::string(string, test_uuid())) {
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
