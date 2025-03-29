use crate::core::parser::*;

use crate::core::either::Either;
use crate::util::parser_gen::ParserGen;
pub trait ParserMethods<'a, A>: ParserTrait<'a, A> 
where 
    A: Clone + 'a
{
    fn and<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<(A, B)>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a;
    
    fn and_left<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<A>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a;
    
    fn and_right<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a;
    
    fn array<B, C>(self, separate_parser: Parser<'a, B>, l_bracket_parser: Parser<'a, C>, r_bracket_parser: Parser<'a, C>) -> Parser<'a, Vec<A>>
    where 
        B: 'a + Clone,
        C: 'a + Clone;
    
    fn bracket<B>(self, l_bracket_parser: Parser<'a, B>, r_bracket_parser: Parser<'a, B>) -> Parser<'a, A>
    where
        B: Clone + 'a;
    
    fn either<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A, B>>
    where
        B: Clone + 'a;

    fn not(self) -> Parser<'a, char>;

    fn optional(self) -> Parser<'a, Option<A>>;

    fn or(self, parser2: Parser<'a, A>) -> Parser<'a, A>;

    fn pure<B>(self, value: B) -> Parser<'a, B>
    where 
        B: Clone + 'a;

    fn separate0<B>(self, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        B: Clone + 'a;
    
    fn separate1<B>(self, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        B: Clone + 'a;

    fn seq0(self) -> Parser<'a, Vec<A>>;
    fn seq1(self) -> Parser<'a, Vec<A>>;

    fn skip(self) -> Parser<'a, ()>;

    fn skip_seq0(self) -> Parser<'a, ()>;

    fn with_end(self) -> Parser<'a, A>;

    fn with_skip_space(self) -> Parser<'a, A>;
    
}

impl <'a, A>ParserMethods<'a, A> for Parser<'a, A>
where 
    A: Clone + 'a
{
    fn and<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<(A, B)>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a
    {
        ParserGen::and(self, parser2)
    }

    fn and_left<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<A>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a
    {
        ParserGen::and(self, parser2).map(|v| v.0)
    }
    
    fn and_right<B>(self, parser2: Parser<'a, B>) -> Self::ParserNext<B>
    where
        // Self::Output: Clone + 'a,
        B: Clone + 'a
    {
        ParserGen::and(self, parser2).map(|v| v.1)
    }

    fn array<B, C>(self, separate_parser: Parser<'a, B>, l_bracket_parser: Parser<'a, C>, r_bracket_parser: Parser<'a, C>) -> Parser<'a, Vec<A>>
    where 
        B: 'a + Clone,
        C: 'a + Clone,
    {
        ParserGen::array(self, separate_parser, l_bracket_parser, r_bracket_parser)
    }

    fn bracket<B>(self, l_bracket_parser: Parser<'a, B>, r_bracket_parser: Parser<'a, B>) -> Parser<'a, A>
    where
        A: Clone + 'a,
        B: Clone + 'a,
    {
        ParserGen::bracket(self, l_bracket_parser, r_bracket_parser)
    }

    fn either<B>(self, parser2: Parser<'a, B>) -> Parser<'a, Either<A, B>>
    where
        B: Clone + 'a
    {
        ParserGen::either(self, parser2)
    }

    fn not(self) -> Parser<'a, char> 
    {

        ParserGen::not(self)
    }

    fn optional(self) -> Parser<'a, Option<A>>
    {
        ParserGen::optional(self)
    }

    fn or(self, parser2: Parser<'a, A>) -> Parser<'a, A>
    {
        ParserGen::or(self, parser2)
    }

    fn pure<B>(self, v: B) -> Parser<'a, B>
    where 
        B: Clone + 'a
    {
        self.map(move |_| v.clone())
    }

    fn separate0<B>(self, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        B: Clone + 'a,
    {
        ParserGen::separate0(self, separate_parser)
    }

    fn separate1<B>(self, separate_parser: Parser<'a, B>) -> Parser<'a, Vec<A>>
    where
        B: Clone + 'a,
    {
        ParserGen::separate1(self, separate_parser)
    }

    fn seq0(self) -> Parser<'a, Vec<A>>
    {
        ParserGen::seq0(self)
    }

    fn seq1(self) -> Parser<'a, Vec<A>>
    {
        ParserGen::seq1(self)
    }

    fn skip(self) -> Parser<'a, ()>
    {
        ParserGen::skip(self)
    }

    fn skip_seq0(self) -> Parser<'a, ()>
    {
        ParserGen::skip_seq0(self)
    }

    fn with_end(self) -> Parser<'a, A>
    {
        self.and_left(ParserGen::end())
    }

    fn with_skip_space(self) -> Parser<'a, A>
    {
        ParserGen::skip_space().and_right(self)
    }
}