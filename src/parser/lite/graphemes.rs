use crate::Tag;
use derive_new::new;
use getset::Getters;
use memchr::Memchr;
use nom::InputIter;
use nom::{
    self, error::ErrorKind, error::ParseError, Compare, IResult, InputLength, InputTake,
    InputTakeAtPosition, Offset,
};
use std::fmt::Debug;
pub use std::str::{CharIndices, Chars};
use unicode_segmentation::{GraphemeIndices, Graphemes, UnicodeSegmentation};
use uuid::Uuid;

#[derive(Eq, PartialEq, Copy, Clone, Debug, new)]
pub struct Source<'a> {
    input: &'a str,
}

impl<'a> Source<'a> {
    fn offset(&self, other: Source<'a>) -> usize {
        self.input.offset(other.input)
    }

    fn as_bytes(&self) -> &'a [u8] {
        self.input.as_bytes()
    }

    fn slice_bytes(&self, from: Option<usize>, to: Option<usize>) -> Source<'a> {
        match (from, to) {
            (Some(from), Some(to)) => Source::new(&self.input[from..to]),
            (Some(from), None) => Source::new(&self.input[from..]),
            (None, Some(to)) => Source::new(&self.input[..to]),
            (None, None) => Source::new(&self.input[..]),
        }
    }

    // TODO: Cache grapheme offsets
    fn slice(&self, from: Option<usize>, to: Option<usize>) -> Source<'a> {
        let iter = self.iter_indices();
        let mut start: Option<usize> = None;
        let mut end: Option<usize> = None;

        for (index, (byte_offset, grapheme)) in iter.enumerate() {
            if let Some(from) = from {
                if from == index {
                    start = Some(byte_offset);
                }
            }

            if let Some(to) = to {
                if to == index {
                    end = Some(byte_offset);
                    break;
                }
            }
        }

        match (start, end) {
            (Some(start), Some(end)) => Source::new(&self.input[start..end]),
            (Some(start), None) => Source::new(&self.input[start..]),
            (None, Some(end)) => Source::new(&self.input[..end]),
            (None, None) => Source::new(&self.input[..]),
        }
    }

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
pub struct GraphemeSpan<'a, X: Copy + Debug> {
    #[get = "pub(crate)"]
    offset: usize,
    #[get = "pub(crate)"]
    fragment: Source<'a>,
    #[get = "pub(crate)"]
    extra: X,
}

impl<'a, X: Copy + Debug> Compare<&str> for GraphemeSpan<'a, X> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.fragment.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.fragment.input.compare_no_case(t)
    }
}

impl<'a, X: Copy + Debug> GraphemeSpan<'a, X> {
    fn slice(&self, from: Option<usize>, to: Option<usize>) -> Self {
        let next_fragment = self.fragment.slice(from, to);
        let consumed_len = self.fragment.offset(next_fragment);

        if consumed_len == 0 {
            return GraphemeSpan {
                offset: self.offset,
                fragment: next_fragment,
                extra: self.extra,
            };
        }

        let consumed = self.fragment.slice(None, Some(consumed_len));
        let next_offset = self.offset + consumed_len;

        GraphemeSpan {
            offset: next_offset,
            fragment: next_fragment,
            extra: self.extra,
        }
    }
}

impl<'a, X: Copy + Debug> GraphemeSpan<'a, X> {
    pub fn string(s: &'a str, extra: X) -> GraphemeSpan<'a, X> {
        GraphemeSpan {
            offset: 0,
            fragment: Source::new(s),
            extra,
        }
    }
}

impl<'a, X: Copy + Debug> InputLength for GraphemeSpan<'a, X> {
    fn input_len(&self) -> usize {
        self.fragment.bytelen()
    }
}

impl<'a, X: Copy + Debug> InputTake for GraphemeSpan<'a, X> {
    fn take(&self, count: usize) -> Self {
        GraphemeSpan::new(
            self.offset,
            self.fragment.slice_bytes(None, Some(count)),
            self.extra,
        )
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let left = self.fragment.slice_bytes(None, Some(count));
        let right = self.fragment.slice_bytes(Some(count), None);

        let out = (
            GraphemeSpan::new(self.offset + count, right, self.extra),
            GraphemeSpan::new(self.offset, left, self.extra),
        );

        println!("take_split :: {:?}", out);

        out
    }
}

impl<'a, X: Copy + Debug> InputTakeAtPosition for GraphemeSpan<'a, X> {
    type Item = Grapheme<'a>;

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.fragment.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::Size(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let result = match self.fragment.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::Size(1))),
        };

        match &result {
            Ok(_) => {}
            Err(nom::Err::Incomplete(_)) => println!("incomplete"),
            Err(nom::Err::Error(_)) => println!("error"),
            Err(nom::Err::Failure(_)) => println!("failure"),
        }

        result
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        // match self.fragment.find(predicate) {
        //     Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
        //     Some(i) => Ok((&self[i..], &self[..i])),
        //     None => {
        //         if self.fragment.len() == 0 {
        //             Err(Err::Error(E::from_error_kind(self, e)))
        //         } else {
        //             Ok(self.take_split(self.input_len()))
        //         }
        //     }
        // }

        match self.fragment.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.fragment.bytelen() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.fragment.bytelen()))
                }
            }
        }
    }
}

impl<'a, X: Copy + Debug> InputIter for GraphemeSpan<'a, X> {
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
    use nom::branch::alt;
    use nom::bytes::complete::{take_while, take_while1};
    use nom::multi::many0;
    use nom::IResult;

    type Input<'a> = GraphemeSpan<'a, ()>;

    #[derive(Eq, PartialEq, Debug)]
    enum Token {
        Word(String),
        Whitespace(String),
    }

    fn words(input: Input) -> IResult<Input, Vec<Token>> {
        let (input, tokens) = many0(alt((word, whitespace)))(input)?;

        println!("{:?}", input);
        println!("{:?}", tokens);

        Ok((input, tokens))
    }

    fn word(input: Input) -> IResult<Input, Token> {
        println!("Trying word for {:?}", input);
        let start = input.offset();
        let (input, string) = take_while1(is_word_char)(input)?;

        println!("Succeeded, new-input :: {:?}", input);

        Ok((input, Token::Word(string.fragment.to_string())))
    }

    fn whitespace(input: Input) -> IResult<Input, Token> {
        println!("Trying whitespace for {:?}", input);

        let start = input.offset();
        let (input, string) = take_while1(is_whitespace_char)(input)?;

        println!("Succeeded, new-input :: {:?}", input);

        Ok((input, Token::Whitespace(string.fragment.to_string())))
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

    #[test]
    fn test_word() -> Result<(), String> {
        let source = GraphemeSpan::string("hello", ());
        let (input, output) = word(source).map_err(|e| format!("{:?}", e))?;
        assert_eq!(output, Token::Word("hello".into()));

        let source = GraphemeSpan::string("héllo", ());
        let (input, output) = word(source).map_err(|e| format!("{:?}", e))?;
        assert_eq!(output, Token::Word("héllo".into()));

        let source = GraphemeSpan::string("héllo", ());
        let (input, output) = word(source).map_err(|e| format!("{:?}", e))?;
        assert_eq!(output, Token::Word("héllo".into()));

        Ok(())
    }

    #[test]
    fn test_multi_words() -> Result<(), String> {
        let source = GraphemeSpan::string("hello world", ());
        let (input, output) = words(source).map_err(|e| format!("{:?}", e))?;

        assert!(input.fragment.bytelen() == 0, "The input should be empty");

        assert_eq!(
            output,
            vec![
                Token::Word("hello".into()),
                Token::Whitespace(" ".into()),
                Token::Word("world".into())
            ]
        );

        // let source = GraphemeSpan::string("héllo", ());
        // let (input, output) = word(source).map_err(|e| format!("{:?}", e))?;
        // assert_eq!(output, Token::Word("héllo".into()));

        // let source = GraphemeSpan::string("héllo", ());
        // let (input, output) = word(source).map_err(|e| format!("{:?}", e))?;
        // assert_eq!(output, Token::Word("héllo".into()));

        Ok(())
    }
}
