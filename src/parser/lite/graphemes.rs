use crate::Tag;
use derive_new::new;
use getset::Getters;
use nom::InputIter;
use nom::{Compare, InputLength, InputTake, UnspecializedInput};
use std::fmt::Debug;
pub use std::str::{CharIndices, Chars};
use unicode_segmentation::{GraphemeIndices, Graphemes, UnicodeSegmentation};
use uuid::Uuid;

#[derive(Eq, PartialEq, Copy, Clone, Debug, new)]
pub struct Source<'a> {
    input: &'a str,
}

impl<'a> Source<'a> {
    fn iter_indices(&self) -> SourceGraphemeIndices<'a> {
        let indices = self.input.grapheme_indices(true);

        SourceGraphemeIndices { indices }
    }

    fn iter_elements(&self) -> SourceGraphemes<'a> {
        let graphemes = self.input.graphemes(true);

        SourceGraphemes { graphemes }
    }

    fn position(&self, predicate: impl Fn(Grapheme<'a>) -> bool) -> Option<usize> {
        self.input
            .graphemes(true)
            .position(|grapheme| predicate(Grapheme { grapheme }))
    }

    fn bytelen(&self) -> usize {
        self.input.len()
    }

    fn to_string(&self) -> String {
        self.input.to_string()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct GraphemeIndex<'a> {
    grapheme: &'a str,
    index: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct Grapheme<'a> {
    grapheme: &'a str,
}

#[derive(Clone)]
pub struct SourceGraphemeIndices<'a> {
    indices: GraphemeIndices<'a>,
}

impl<'a> Iterator for SourceGraphemeIndices<'a> {
    type Item = (usize, Grapheme<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        self.indices
            .next()
            .map(|(index, grapheme)| (index, Grapheme { grapheme }))
    }
}

#[derive(Clone)]
pub struct SourceGraphemes<'a> {
    graphemes: Graphemes<'a>,
}

impl<'a> Iterator for SourceGraphemes<'a> {
    type Item = Grapheme<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.graphemes.next().map(|grapheme| Grapheme { grapheme })
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Getters, new)]
pub struct GraphemeSpan<'a, X> {
    #[new(default)]
    #[get = "pub(crate)"]
    offset: usize,
    #[get = "pub(crate)"]
    fragment: Source<'a>,
    extra: X,
}

impl<'a, X> GraphemeSpan<'a, X> {
    pub fn string(s: &'a str, extra: X) -> GraphemeSpan<'a, X> {
        GraphemeSpan {
            offset: 0,
            fragment: Source::new(s),
            extra,
        }
    }
}

impl<'a, X> InputLength for GraphemeSpan<'a, X> {
    fn input_len(&self) -> usize {
        unimplemented!()
    }
}

impl<'a, X> InputTake for GraphemeSpan<'a, X> {
    fn take(&self, count: usize) -> Self {
        unimplemented!()
    }
    fn take_split(&self, count: usize) -> (Self, Self) {
        unimplemented!()
    }
}

impl<'a, X> UnspecializedInput for GraphemeSpan<'a, X> {}

impl<'a, X> InputIter for GraphemeSpan<'a, X> {
    type Item = Grapheme<'a>;
    type Iter = SourceGraphemeIndices<'a>;
    type IterElem = SourceGraphemes<'a>;

    #[inline]
    fn iter_indices(&self) -> SourceGraphemeIndices<'a> {
        self.fragment.iter_indices()
    }

    #[inline]
    fn iter_elements(&self) -> SourceGraphemes<'a> {
        self.fragment.iter_elements()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Grapheme<'a>) -> bool,
    {
        self.fragment.position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        let mut cnt = 0;
        for (index, _) in self.iter_indices() {
            if cnt == count {
                return Some(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Some(self.fragment.bytelen());
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::bytes::complete::take_while;
    use nom::IResult;

    type Input<'a> = GraphemeSpan<'a, ()>;

    fn word(input: Input) -> IResult<Input, String> {
        let start = input.offset();
        let (input, string) = take_while(is_word_char)(input)?;

        Ok((input, string.fragment.to_string()))
    }

    fn is_word_char(c: Grapheme) -> bool {
        match c.grapheme {
            "|" | "$" | "\"" | "'" | "#" => false,
            other => other.chars().all(|c| c.is_whitespace()),
        }
    }

    #[test]
    fn test_word() -> Result<(), String> {
        let source = GraphemeSpan::string("hello", ());

        let (input, word) = word(source).map_err(|e| format!("{:?}", e))?;

        assert_eq!(word, "hello");

        Ok(())
    }
}
