pub enum Either<A, B> {
    Left(A), Right(B)
}

impl<A, B> Clone for Either<A, B>
where
    A: Clone,
    B: Clone
{
    fn clone(&self) -> Self { 
        match self {
            Either::Left(a) => Either::Left(a.clone()),
            Either::Right(b) => Either::Right(b.clone())
        }
    }
}